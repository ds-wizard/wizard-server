module Shared.Service.Bootstrap.BootstrapMapper where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as M

import Shared.Api.Resource.Bootstrap.BootstrapCommonDTO
import Shared.Api.Resource.Bootstrap.WorkspaceBootstrapDTO
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.SimpleFeature
import Shared.Model.Settings.Settings
import Shared.Model.Tenant.Module.TenantModule
import Shared.Model.Tenant.Tenant
import Shared.Model.Workspace.Workspace
import Shared.Service.Tenant.TenantMapper (tenantServerUrl)

toPrivacyDTO :: Maybe String -> Maybe String -> BootstrapPrivacyDTO
toPrivacyDTO privacyUrl termsOfServiceUrl = BootstrapPrivacyDTO {privacyUrl = privacyUrl, termsOfServiceUrl = termsOfServiceUrl}

toCloudDTO :: ServerConfigCloud -> Tenant -> BootstrapCloudDTO
toCloudDTO serverConfig tenant = BootstrapCloudDTO {enabled = serverConfig.enabled, serverUrl = tenantServerUrl tenant}

toSignalBridgeDTO :: ServerConfigCloud -> BootstrapSignalBridgeDTO
toSignalBridgeDTO serverConfig = BootstrapSignalBridgeDTO {webSocketUrl = serverConfig.signalBridgeUrl}

toModuleDTOs :: [String] -> [TenantModule] -> [BootstrapModuleDTO]
toModuleDTOs permissions tenantModules =
  [toModuleDTO m | m <- tenantModules, m.enabled, maybe True (`elem` permissions) m.requiredPermission]

toModuleDTO :: TenantModule -> BootstrapModuleDTO
toModuleDTO tenantModule =
  BootstrapModuleDTO
    { title = tenantModule.title
    , description = tenantModule.description
    , icon = tenantModule.icon
    , url = tenantModule.url
    , external = tenantModule.external
    }

toWorkspaceBootstrapDTO :: Workspace -> SettingsLookAndFeel -> SettingsDashboardAndMenu -> SettingsProjects -> SettingsSupport -> SettingsSubmission -> WorkspaceBootstrapDTO
toWorkspaceBootstrapDTO workspace lookAndFeel dashboardAndMenu projects support submission =
  WorkspaceBootstrapDTO
    { logo = workspace.logo <|> lookAndFeel.logoUrl
    , primaryColor = workspace.primaryColor <|> lookAndFeel.primaryColor
    , dashboardAndMenu = dashboardAndMenu
    , projects = projects
    , support = support
    , submission = SimpleFeature submission.enabled
    , plugins = []
    , pluginSettings = M.empty
    }
