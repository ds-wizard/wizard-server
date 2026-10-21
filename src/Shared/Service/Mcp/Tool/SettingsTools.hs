module Shared.Service.Mcp.Tool.SettingsTools where

import Control.Monad (when)
import Control.Monad.Except (liftEither, throwError)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Proxy

import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Context.AclContext
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpSettingsSection
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Mcp.Tool.SettingsSecretMapper
import Shared.Service.Settings.OrganizationSettingsService
import Shared.Service.Settings.SettingsRolesService
import Shared.Service.Settings.WorkspaceSettingsService
import Shared.Util.JsonSchema

settingsTools :: WizardRequestContextC s m => [McpSettingsSection m] -> [McpTool m]
settingsTools sections = [getSettingsTool sections, updateSettingsTool sections]

sharedSettingsSections :: WizardRequestContextC s m => [McpSettingsSection m]
sharedSettingsSections =
  [ (settingsSection "authentication" getSettingsAuthentication modifySettingsAuthentication) {sensitive = True, sensitiveRead = True}
  , settingsSection "users" getSettingsUsers modifySettingsUsers
  , (settingsSection "roles" getSettingsRoles modifySettingsRoles) {sensitive = True}
  , settingsSection "login-screen" getSettingsLoginScreen modifySettingsLoginScreen
  , settingsSection "features" getSettingsFeatures modifySettingsFeatures
  , settingsSection "look-and-feel" getSettingsLookAndFeel modifySettingsLookAndFeel
  , overridableSection "dashboard-and-menu" getSettingsDashboardAndMenu modifySettingsDashboardAndMenu deleteSettingsDashboardAndMenu
  , overridableSection "projects" getSettingsProjects modifySettingsProjects deleteSettingsProjects
  , overridableSection "support" getSettingsSupport modifySettingsSupport deleteSettingsSupport
  , (maskedSettingsSection "submission" maskSubmission unmaskSubmission getSettingsSubmission modifySettingsSubmission)
      { sensitive = True
      , resetOverride = Just deleteSettingsSubmission
      }
  ]

settingsSection
  :: forall s m a
   . (WizardRequestContextC s m, ToJSON a, FromJSON a, JsonSchema a)
  => String
  -> m (SettingsDTO a)
  -> (SettingsDTO a -> m (SettingsDTO a))
  -> McpSettingsSection m
settingsSection name = maskedSettingsSection name id (const Right)

maskedSettingsSection
  :: forall s m a
   . (WizardRequestContextC s m, ToJSON a, FromJSON a, JsonSchema a)
  => String
  -> (a -> a)
  -> (a -> a -> Either AppError a)
  -> m (SettingsDTO a)
  -> (SettingsDTO a -> m (SettingsDTO a))
  -> McpSettingsSection m
maskedSettingsSection name mask unmask getSettings modifySettings =
  McpSettingsSection
    { name = name
    , valueSchema = jsonSchema (Proxy :: Proxy a)
    , sensitive = False
    , sensitiveRead = False
    , getSection = toJSON . maskSettings mask <$> getSettings
    , updateSection = \changes mOverrideAllowed -> toJSON . maskSettings mask <$> mergeSettings unmask getSettings modifySettings changes mOverrideAllowed
    , resetOverride = Nothing
    }

maskSettings :: (a -> a) -> SettingsDTO a -> SettingsDTO a
maskSettings mask settings = settings {value = mask settings.value}

overridableSection
  :: (WizardRequestContextC s m, ToJSON a, FromJSON a, JsonSchema a)
  => String
  -> m (SettingsDTO a)
  -> (SettingsDTO a -> m (SettingsDTO a))
  -> m ()
  -> McpSettingsSection m
overridableSection name getSettings modifySettings deleteSettings = (settingsSection name getSettings modifySettings) {resetOverride = Just deleteSettings}

mergeSettings
  :: (WizardRequestContextC s m, ToJSON a, FromJSON a)
  => (a -> a -> Either AppError a)
  -> m (SettingsDTO a)
  -> (SettingsDTO a -> m (SettingsDTO a))
  -> Object
  -> Maybe Bool
  -> m (SettingsDTO a)
mergeSettings unmask getSettings modifySettings changes mOverrideAllowed = do
  current <- getSettings
  value <- liftEither . unmask current.value =<< mergeNestedChanges changes current.value
  modifySettings (SettingsDTO value mOverrideAllowed Nothing)

getSettingsTool :: forall s m. WizardRequestContextC s m => [McpSettingsSection m] -> McpTool m
getSettingsTool sections =
  McpTool
    { name = "get_settings"
    , title = "Get settings"
    , description =
        "Get one settings section of the organization (tenant: true) or of a workspace (workspace_uuid). "
          ++ "In a workspace, dashboard-and-menu, projects, support and submission show the override of the workspace when it has one (overridden) and the organization value otherwise, "
          ++ "and overrideAllowed tells whether workspaces may override them; roles holds the default role of new members of the organization or of the workspace; "
          ++ "the other sections exist only at the organization level. Secrets (the header values of the submission services, "
          ++ "the API key of registry) show as ********. Reading authentication needs mcp.sensitive"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ settingsSchema sections
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION, settingsPermission]
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(inScope, section) -> inScope (getSettings section)
    }
  where
    arguments = (,) <$> scopeArguments <*> sectionArgument sections :: Arguments (m Value -> m Value, McpSettingsSection m)

updateSettingsTool :: forall s m. WizardRequestContextC s m => [McpSettingsSection m] -> McpTool m
updateSettingsTool sections =
  McpTool
    { name = "update_settings"
    , title = "Update settings"
    , description =
        "Change one settings section of the organization (tenant: true) or of a workspace (workspace_uuid): only the fields you give in value change, "
          ++ "nested objects are merged field by field and lists are replaced as a whole. A change in a workspace creates or changes its override of "
          ++ "dashboard-and-menu, projects, support or submission (when the organization allows overrides); reset_override: true removes that override, so that the workspace "
          ++ "follows the organization again. override_allowed decides at the organization level whether workspaces may override the section; false removes their overrides. "
          ++ "Changing authentication, roles, submission or registry needs mcp.sensitive. The header values of the submission services and the API key of registry are secrets: "
          ++ "they show as ******** and ******** sent back keeps the stored value, except for a submission service whose request url changes in the same call"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ describedSchema "The section, shaped as get_settings returns it" (typeSchema "object")
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, settingsPermission]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(inScope, section, changes, mOverrideAllowed, mReset) ->
        inScope (updateSettings section changes mOverrideAllowed (fromMaybe False mReset))
    }
  where
    arguments =
      (,,,,)
        <$> scopeArguments
        <*> sectionArgument sections
        <*> valueArgument sections
        <*> optionalArgument "override_allowed" "At the organization level of dashboard-and-menu, projects, support or submission: whether workspaces may override the section"
        <*> optionalArgument "reset_override" "Remove the override of the workspace instead of changing it"
        :: Arguments (m Value -> m Value, McpSettingsSection m, Object, Maybe Bool, Maybe Bool)

getSettings :: WizardRequestContextC s m => McpSettingsSection m -> m Value
getSettings section = do
  when section.sensitiveRead (checkPermission _MCP_SENSITIVE_ROLE_PERMISSION)
  section.getSection

updateSettings :: WizardRequestContextC s m => McpSettingsSection m -> Object -> Maybe Bool -> Bool -> m Value
updateSettings section changes mOverrideAllowed reset = do
  when section.sensitive (checkPermission _MCP_SENSITIVE_ROLE_PERMISSION)
  if reset
    then resetSettings section
    else section.updateSection changes mOverrideAllowed

resetSettings :: WizardRequestContextC s m => McpSettingsSection m -> m Value
resetSettings section =
  case section.resetOverride of
    Just resetOverride -> resetOverride >> section.getSection
    Nothing -> throwError . UserError $ _ERROR_SERVICE_MCP__SETTINGS_NOT_OVERRIDABLE section.name

settingsPermission :: String
settingsPermission = anyOfPermissions [_SETTINGS_MANAGE_ROLE_PERMISSION, _WORKSPACE_SETTINGS_MANAGE_ROLE_PERMISSION]

sectionArgument :: [McpSettingsSection m] -> Arguments (McpSettingsSection m)
sectionArgument sections =
  withArgumentSchema (describedSchema "The settings section" (enumSchema names)) $
    checkArguments findSection (requiredArgument "section" "")
  where
    names = fmap (.name) sections
    findSection name =
      maybe (Left $ _ERROR_VALIDATION__MCP_SETTINGS_SECTION name names) Right (find ((== name) . (.name)) sections)

valueArgument :: [McpSettingsSection m] -> Arguments Object
valueArgument sections =
  withArgumentSchema (describedSchema "The fields of the section to change" (anyOfSchema (fmap (partialSchema . sectionSchema) sections))) $
    fromMaybe KM.empty <$> optionalArgument "value" ""

settingsSchema :: [McpSettingsSection m] -> Value
settingsSchema sections = withProperties [("value", anyOfSchema (fmap sectionSchema sections))] (jsonSchema (Proxy :: Proxy (SettingsDTO Value)))

sectionSchema :: McpSettingsSection m -> Value
sectionSchema section = describedSchema ("The " ++ section.name ++ " section") section.valueSchema
