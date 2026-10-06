module Shared.Database.Migration.Development.Plugin.Data.WorkspacePluginSettings where

import Shared.Constant.Tenant
import Shared.Constant.Workspace
import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.WorkspacePluginSettings
import Shared.Util.Date

defaultWorkspacePluginSettingsDisabled :: WorkspacePluginSettings
defaultWorkspacePluginSettingsDisabled =
  WorkspacePluginSettings
    { workspaceUuid = defaultWorkspaceUuid
    , pluginUuid = plugin1.uuid
    , tenantUuid = defaultTenantUuid
    , enabled = False
    , values = Nothing
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

defaultWorkspacePluginSettingsOverridden :: WorkspacePluginSettings
defaultWorkspacePluginSettingsOverridden =
  defaultWorkspacePluginSettingsDisabled
    { enabled = True
    , values = Just plugin1Values2
    }

secondWorkspacePluginSettingsOverridden :: WorkspacePluginSettings
secondWorkspacePluginSettingsOverridden =
  defaultWorkspacePluginSettingsOverridden
    { workspaceUuid = secondWorkspaceUuid
    }
