module Shared.Service.Settings.OrganizationSettingsService where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Database.DAO.Settings.SettingsAuthenticationDAO
import Shared.Database.DAO.Settings.SettingsFeaturesDAO
import Shared.Database.DAO.Settings.SettingsLoginScreenDAO
import Shared.Database.DAO.Settings.SettingsLookAndFeelDAO
import Shared.Database.DAO.Settings.SettingsRegistryDAO
import Shared.Database.DAO.Settings.SettingsUsersDAO
import Shared.Model.Common.SensitiveData
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings
import Shared.Model.Settings.SettingsEM ()
import Shared.Service.Settings.SettingsAuditLog
import Shared.Service.Settings.SettingsService

getSettingsAuthentication :: WizardRequestContextC s m => m (SettingsDTO SettingsAuthentication)
getSettingsAuthentication = getOrganizationSettings findSettingsAuthentication

modifySettingsAuthentication :: WizardRequestContextC s m => SettingsDTO SettingsAuthentication -> m (SettingsDTO SettingsAuthentication)
modifySettingsAuthentication = modifyOrganizationSettings (OrganizationSettings findSettingsAuthentication saveSettingsAuthentication (al_updateSection "authentication" authenticationChanges))

getSettingsUsers :: WizardRequestContextC s m => m (SettingsDTO SettingsUsers)
getSettingsUsers = getOrganizationSettings findSettingsUsers

modifySettingsUsers :: WizardRequestContextC s m => SettingsDTO SettingsUsers -> m (SettingsDTO SettingsUsers)
modifySettingsUsers = modifyOrganizationSettings (OrganizationSettings findSettingsUsers saveSettingsUsers (al_updateSection "users" usersChanges))

getSettingsLoginScreen :: WizardRequestContextC s m => m (SettingsDTO SettingsLoginScreen)
getSettingsLoginScreen = getOrganizationSettings findSettingsLoginScreen

modifySettingsLoginScreen :: WizardRequestContextC s m => SettingsDTO SettingsLoginScreen -> m (SettingsDTO SettingsLoginScreen)
modifySettingsLoginScreen = modifyOrganizationSettings (OrganizationSettings findSettingsLoginScreen saveSettingsLoginScreen (al_updateSection "loginScreen" loginScreenChanges))

getSettingsFeatures :: WizardRequestContextC s m => m (SettingsDTO SettingsFeatures)
getSettingsFeatures = getOrganizationSettings findSettingsFeatures

modifySettingsFeatures :: WizardRequestContextC s m => SettingsDTO SettingsFeatures -> m (SettingsDTO SettingsFeatures)
modifySettingsFeatures = modifyOrganizationSettings (OrganizationSettings findSettingsFeatures saveSettingsFeatures (al_updateSection "features" featuresChanges))

getSettingsLookAndFeel :: WizardRequestContextC s m => m (SettingsDTO SettingsLookAndFeel)
getSettingsLookAndFeel = getOrganizationSettings findSettingsLookAndFeel

modifySettingsLookAndFeel :: WizardRequestContextC s m => SettingsDTO SettingsLookAndFeel -> m (SettingsDTO SettingsLookAndFeel)
modifySettingsLookAndFeel = modifyOrganizationSettings (OrganizationSettings findSettingsLookAndFeel saveSettingsLookAndFeel (al_updateSection "lookAndFeel" lookAndFeelChanges))

getSettingsRegistry :: WizardRequestContextC s m => m (SettingsDTO SettingsRegistry)
getSettingsRegistry = getOrganizationSettings findDecryptedSettingsRegistry

modifySettingsRegistry :: WizardRequestContextC s m => SettingsDTO SettingsRegistry -> m (SettingsDTO SettingsRegistry)
modifySettingsRegistry = modifyOrganizationSettings (OrganizationSettings findDecryptedSettingsRegistry saveEncryptedSettingsRegistry al_updateRegistrySettings)

getCurrentSettingsRegistry :: WizardRequestContextC s m => m SettingsRegistry
getCurrentSettingsRegistry = getCurrentSettings findDecryptedSettingsRegistry

findDecryptedSettingsRegistry :: WizardRequestContextC s m => U.UUID -> m (Maybe SettingsRegistry)
findDecryptedSettingsRegistry tenantUuid = do
  serverConfig <- asks (.serverConfig')
  fmap (process serverConfig.general.secret) <$> findSettingsRegistry tenantUuid

saveEncryptedSettingsRegistry :: WizardRequestContextC s m => U.UUID -> SettingsRegistry -> m ()
saveEncryptedSettingsRegistry tenantUuid settings = do
  serverConfig <- asks (.serverConfig')
  saveSettingsRegistry tenantUuid (process serverConfig.general.secret settings)
