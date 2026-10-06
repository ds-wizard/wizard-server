module Shared.Service.Bootstrap.BootstrapMapper where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.UUID as U

import Shared.Api.Resource.Bootstrap.BootstrapCommonDTO
import Shared.Api.Resource.Bootstrap.WorkspaceBootstrapDTO
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.SimpleFeature
import Shared.Model.Plugin.EffectivePlugin
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.PluginList
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

toBootstrapPlugins :: Bool -> [PluginList] -> M.Map U.UUID A.Value -> BootstrapPlugins
toBootstrapPlugins signedIn plugins pluginSettings =
  BootstrapPlugins
    { plugins = plugins
    , pluginSettings = if signedIn then M.restrictKeys pluginSettings (S.fromList [p.uuid | p <- plugins, p.enabled]) else M.empty
    }

toWorkspaceBootstrapDTO :: Workspace -> SettingsLookAndFeel -> SettingsDashboardAndMenu -> SettingsProjects -> SettingsSupport -> SettingsSubmission -> [EffectivePlugin] -> WorkspaceBootstrapDTO
toWorkspaceBootstrapDTO workspace lookAndFeel dashboardAndMenu projects support submission effectivePlugins =
  WorkspaceBootstrapDTO
    { logo = workspace.logo <|> lookAndFeel.logoUrl
    , primaryColor = workspace.primaryColor <|> lookAndFeel.primaryColor
    , dashboardAndMenu = dashboardAndMenu
    , projects = projects
    , support = support
    , submission = SimpleFeature submission.enabled
    , disabledPlugins = [p.plugin.uuid | p <- effectivePlugins, not p.enabled]
    , pluginSettings = M.fromList [(p.plugin.uuid, values) | p <- effectivePlugins, p.enabled, Just values <- [p.values]]
    }
