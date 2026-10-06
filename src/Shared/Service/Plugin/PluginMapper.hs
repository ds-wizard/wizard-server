module Shared.Service.Plugin.PluginMapper where

import qualified Data.Aeson as A
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Model.Plugin.EffectivePlugin
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.TenantPluginSettings

toPlugin :: U.UUID -> String -> Bool -> U.UUID -> UTCTime -> Plugin
toPlugin uuid url enabled tenantUuid now =
  Plugin
    { uuid = uuid
    , url = url
    , enabled = enabled
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    , workspaceOverrideAllowed = True
    }

toTenantPluginSettings :: U.UUID -> U.UUID -> A.Value -> UTCTime -> TenantPluginSettings
toTenantPluginSettings tenantUuid pluginUuid values now =
  TenantPluginSettings
    { tenantUuid = tenantUuid
    , pluginUuid = pluginUuid
    , values = values
    , createdAt = now
    , updatedAt = now
    }

toOrganizationPluginSettingsListDTO :: Bool -> Plugin -> PluginSettingsListDTO
toOrganizationPluginSettingsListDTO multiWorkspace plugin =
  PluginSettingsListDTO
    { pluginUuid = plugin.uuid
    , enabled = plugin.enabled
    , workspaceOverrideAllowed = if multiWorkspace then Just plugin.workspaceOverrideAllowed else Nothing
    , overrideAllowed = Nothing
    , overridden = Nothing
    }

toWorkspacePluginSettingsListDTO :: EffectivePlugin -> PluginSettingsListDTO
toWorkspacePluginSettingsListDTO effective =
  PluginSettingsListDTO
    { pluginUuid = effective.plugin.uuid
    , enabled = effective.enabled
    , workspaceOverrideAllowed = Nothing
    , overrideAllowed = Just effective.plugin.workspaceOverrideAllowed
    , overridden = Just effective.overridden
    }

toWorkspacePluginSettingsDTO :: EffectivePlugin -> A.Value -> WorkspacePluginSettingsDTO
toWorkspacePluginSettingsDTO effective values =
  WorkspacePluginSettingsDTO
    { overridden = effective.overridden
    , overrideAllowed = effective.plugin.workspaceOverrideAllowed
    , values = values
    }
