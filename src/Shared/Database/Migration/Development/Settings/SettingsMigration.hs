module Shared.Database.Migration.Development.Settings.SettingsMigration where

import qualified Data.UUID as U

import Shared.Constant.Tenant
import Shared.Database.DAO.Settings.SettingsAuthenticationDAO
import Shared.Database.DAO.Settings.SettingsDashboardAndMenuDAO
import Shared.Database.DAO.Settings.SettingsFeaturesDAO
import Shared.Database.DAO.Settings.SettingsLoginScreenDAO
import Shared.Database.DAO.Settings.SettingsLookAndFeelDAO
import Shared.Database.DAO.Settings.SettingsProjectsDAO
import Shared.Database.DAO.Settings.SettingsRegistryDAO
import Shared.Database.DAO.Settings.SettingsRolesDAO
import Shared.Database.DAO.Settings.SettingsSubmissionDAO
import Shared.Database.DAO.Settings.SettingsSupportDAO
import Shared.Database.DAO.Settings.SettingsUsersDAO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

seedSettings :: WizardRequestContextC s m => U.UUID -> m ()
seedSettings tenantUuid = do
  saveSettingsAuthentication tenantUuid settingsAuthentication
  saveSettingsUsers tenantUuid settingsUsers
  saveSettingsRoles tenantUuid settingsRoles
  saveSettingsLoginScreen tenantUuid settingsLoginScreen
  saveSettingsRegistry tenantUuid settingsRegistryEncrypted
  saveSettingsFeatures tenantUuid settingsFeatures
  saveSettingsLookAndFeel tenantUuid settingsLookAndFeel
  saveSettingsDashboardAndMenu tenantUuid Nothing settingsDashboardAndMenu
  saveSettingsProjects tenantUuid Nothing settingsProjects
  saveSettingsSupport tenantUuid Nothing settingsSupport
  saveSettingsSubmission tenantUuid Nothing (settingsSubmission {services = []})

seedSettingsSubmissionService :: WizardRequestContextC s m => m ()
seedSettingsSubmissionService = saveSettingsSubmission defaultTenantUuid Nothing settingsSubmission
