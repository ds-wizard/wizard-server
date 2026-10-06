module WizardServer.Api.Resource.Bootstrap.BootstrapDTO where

import GHC.Generics

import Shared.Api.Resource.Bootstrap.BootstrapCommonDTO
import Shared.Model.OpenId.OpenIdClientSimple
import Shared.Model.Settings.Settings
import Shared.Model.User.UserProfile

data BootstrapDTO
  = HousekeepingInProgressBootstrapDTO
      { message :: String
      }
  | BootstrapDTO
      { user :: Maybe UserProfile
      , tours :: [String]
      , authentication :: BootstrapAuthenticationDTO
      , loginScreen :: SettingsLoginScreen
      , dashboardAndMenu :: SettingsDashboardAndMenu
      , features :: SettingsFeatures
      , privacy :: BootstrapPrivacyDTO
      , lookAndFeel :: SettingsLookAndFeel
      , users :: SettingsUsers
      , registry :: BootstrapRegistryDTO
      , cloud :: BootstrapCloudDTO
      , admin :: BootstrapAdminDTO
      , signalBridge :: BootstrapSignalBridgeDTO
      , modules :: [BootstrapModuleDTO]
      , multiWorkspace :: Bool
      }
  deriving (Show, Eq, Generic)

data BootstrapAuthenticationDTO = BootstrapAuthenticationDTO
  { internal :: SettingsAuthentication
  , openId :: [OpenIdClientSimple]
  }
  deriving (Generic, Eq, Show)

data BootstrapRegistryDTO = BootstrapRegistryDTO
  { enabled :: Bool
  , url :: String
  }
  deriving (Generic, Eq, Show)

data BootstrapAdminDTO = BootstrapAdminDTO
  { enabled :: Bool
  }
  deriving (Generic, Eq, Show)
