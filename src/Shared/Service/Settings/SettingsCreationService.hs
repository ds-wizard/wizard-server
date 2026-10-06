module Shared.Service.Settings.SettingsCreationService where

import qualified Data.UUID as U

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
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings
import Shared.Model.Settings.SettingsDM

createSettings :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
createSettings tenantUuid defaultRoleUuid = do
  saveSettingsAuthentication tenantUuid defaultSettingsAuthentication
  saveSettingsUsers tenantUuid defaultSettingsUsers
  saveSettingsRoles tenantUuid (SettingsRoles defaultRoleUuid)
  saveSettingsLoginScreen tenantUuid defaultSettingsLoginScreen
  saveSettingsRegistry tenantUuid defaultSettingsRegistry
  saveSettingsFeatures tenantUuid defaultSettingsFeatures
  saveSettingsLookAndFeel tenantUuid defaultSettingsLookAndFeel
  saveSettingsDashboardAndMenu tenantUuid Nothing defaultSettingsDashboardAndMenu
  saveSettingsProjects tenantUuid Nothing defaultSettingsProjects
  saveSettingsSupport tenantUuid Nothing defaultSettingsSupport
  saveSettingsSubmission tenantUuid Nothing defaultSettingsSubmission
