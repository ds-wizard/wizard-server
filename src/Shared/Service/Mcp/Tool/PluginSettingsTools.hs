module Shared.Service.Mcp.Tool.PluginSettingsTools where

import Control.Monad (void, when)
import Data.Aeson
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Api.Resource.Plugin.PluginSettingsJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Mcp.Tool.SettingsTools (settingsPermission)
import Shared.Service.Plugin.PluginSettingsService
import Shared.Service.Workspace.WorkspaceScopeService (localScope)
import Shared.Util.JsonSchema

pluginSettingsTools :: WizardRequestContextC s m => [McpTool m]
pluginSettingsTools = [listPluginSettingsTool, getPluginSettingsTool, updatePluginSettings]

listPluginSettingsTool :: WizardRequestContextC s m => McpTool m
listPluginSettingsTool =
  McpTool
    { name = "list_plugin_settings"
    , title = "List plugin settings"
    , description =
        "List the plugins of the organization (tenant: true) or of a workspace (workspace_uuid) with whether they are enabled, "
          ++ "whether workspaces may override them (workspaceOverrideAllowed) and whether the workspace does (overridden)"
    , inputSchema = argumentsSchema scopeValueArguments
    , outputSchema = Just $ objectSchema ["plugins"] [("plugins", arraySchema (jsonSchema (Proxy :: Proxy PluginSettingsListDTO)))]
    , annotations = readOnlyAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, settingsPermission]
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments scopeValueArguments $ \scope -> (\plugins -> object ["plugins" .= plugins]) <$> inPluginScope scope getPluginSettingsList
    }

getPluginSettingsTool :: WizardRequestContextC s m => McpTool m
getPluginSettingsTool =
  McpTool
    { name = "get_plugin_settings"
    , title = "Get plugin settings"
    , description =
        "Get the settings values of a plugin in the organization (tenant: true), or the values in effect in a workspace (workspace_uuid) (overridden tells whether they are its own). "
          ++ "The values are free JSON defined by the plugin and may hold secrets"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just pluginValuesSchema
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION, settingsPermission]
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(pluginUuid, scope) -> inPluginScope scope (pluginValues (fst scope) <$> getPluginSettings pluginUuid)
    }
  where
    arguments = (,) <$> pluginUuidArgument <*> scopeValueArguments

pluginValues :: Maybe U.UUID -> Value -> Value
pluginValues Nothing values = object ["values" .= values]
pluginValues (Just _) settings = settings

pluginValuesSchema :: Value
pluginValuesSchema =
  objectSchema
    ["values"]
    [ ("values", describedSchema "The settings values defined by the plugin" (jsonSchema (Proxy :: Proxy Value)))
    , ("overridden", typeSchema "boolean")
    , ("overrideAllowed", typeSchema "boolean")
    ]

updatePluginSettings :: forall s m. WizardRequestContextC s m => McpTool m
updatePluginSettings =
  McpTool
    { name = "update_plugin_settings"
    , title = "Update plugin settings"
    , description =
        "Change a plugin in the organization (tenant: true) or in a workspace (workspace_uuid): enabled switches it on or off, workspace_override_allowed (organization only) decides "
          ++ "whether workspaces may override its values, values replaces its settings values as a whole and reset_override (workspace only) removes the override of the workspace. "
          ++ "It returns the plugin afterwards"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ objectSchema ["plugin"] [("plugin", jsonSchema (Proxy :: Proxy PluginSettingsListDTO))]
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION, settingsPermission]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(pluginUuid, scope, change) -> inPluginScope scope (updatePlugin pluginUuid change)
    }
  where
    arguments = (,,) <$> pluginUuidArgument <*> scopeValueArguments <*> changeArguments
    changeArguments =
      PluginChange
        <$> optionalArgument "enabled" "Switch the plugin on or off"
        <*> optionalArgument "workspace_override_allowed" "At the organization level: whether workspaces may override the values"
        <*> optionalArgument "values" "The new settings values, replacing the current ones"
        <*> (fromMaybe False <$> optionalArgument "reset_override" "Remove the override of the workspace")

updatePlugin :: WizardRequestContextC s m => U.UUID -> PluginChange -> m Value
updatePlugin pluginUuid change = do
  when change.resetOverride (deletePluginSettings pluginUuid)
  when (isJust change.enabled || isJust change.workspaceOverrideAllowed) $
    modifyPluginSettingsListEntry pluginUuid change.enabled change.workspaceOverrideAllowed
  traverse_ (void . modifyPluginSettings pluginUuid) change.values
  (\plugin -> object ["plugin" .= plugin]) <$> getPluginSettingsListEntry pluginUuid

data PluginChange = PluginChange
  { enabled :: Maybe Bool
  , workspaceOverrideAllowed :: Maybe Bool
  , values :: Maybe Value
  , resetOverride :: Bool
  }

inPluginScope :: WizardRequestContextC s m => (Maybe U.UUID, Maybe Bool) -> m a -> m a
inPluginScope = uncurry localScope

pluginUuidArgument :: Arguments U.UUID
pluginUuidArgument = requiredArgument "plugin_uuid" "The plugin"
