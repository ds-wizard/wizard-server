module Shared.Api.Resource.Bootstrap.BootstrapSM where

import qualified Data.Map.Strict as M
import Data.Swagger

import Shared.Api.Resource.Bootstrap.BootstrapCommonDTO
import Shared.Api.Resource.Bootstrap.BootstrapJM ()
import Shared.Api.Resource.Bootstrap.WorkspaceBootstrapDTO
import Shared.Api.Resource.Common.AesonSM ()
import Shared.Api.Resource.Config.SimpleFeatureSM ()
import Shared.Api.Resource.Settings.SettingsSM ()
import Shared.Model.Config.SimpleFeature
import Shared.Model.Settings.SettingsDM
import Shared.Util.Swagger

instance ToSchema BootstrapPrivacyDTO where
  declareNamedSchema = toSwagger (BootstrapPrivacyDTO (Just "https://example.com/privacy") (Just "https://example.com/terms"))

instance ToSchema BootstrapCloudDTO where
  declareNamedSchema = toSwagger (BootstrapCloudDTO True "https://example.com/api")

instance ToSchema BootstrapSignalBridgeDTO where
  declareNamedSchema = toSwagger (BootstrapSignalBridgeDTO Nothing)

instance ToSchema BootstrapModuleDTO where
  declareNamedSchema = toSwagger (BootstrapModuleDTO "Wizard" "Projects" "fas fa-hat-wizard" "https://example.com" False)

instance ToSchema WorkspaceBootstrapDTO where
  declareNamedSchema =
    toSwagger
      ( WorkspaceBootstrapDTO
          { logo = Nothing
          , primaryColor = Nothing
          , dashboardAndMenu = defaultSettingsDashboardAndMenu
          , projects = defaultSettingsProjects
          , support = defaultSettingsSupport
          , submission = SimpleFeature False
          , disabledPlugins = []
          , pluginSettings = M.empty
          }
      )
