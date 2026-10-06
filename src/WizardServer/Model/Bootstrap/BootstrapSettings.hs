module WizardServer.Model.Bootstrap.BootstrapSettings where

import Shared.Model.Settings.Settings

data BootstrapSettings = BootstrapSettings
  { authentication :: SettingsAuthentication
  , loginScreen :: SettingsLoginScreen
  , dashboardAndMenu :: SettingsDashboardAndMenu
  , features :: SettingsFeatures
  , lookAndFeel :: SettingsLookAndFeel
  , users :: SettingsUsers
  , registry :: SettingsRegistry
  }
