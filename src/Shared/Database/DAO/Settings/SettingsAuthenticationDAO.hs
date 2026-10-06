module Shared.Database.DAO.Settings.SettingsAuthenticationDAO where

import qualified Data.UUID as U

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsAuthentication ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_authentication"

columns :: [String]
columns = ["registration_enabled", "non_admin_login_enabled", "session_expiration", "user_email_link_expiration", "two_factor_auth_enabled", "two_factor_auth_code_length", "two_factor_auth_code_expiration"]

findSettingsAuthentication :: WizardRequestContextC s m => U.UUID -> m (Maybe SettingsAuthentication)
findSettingsAuthentication tenantUuid = findSettingsRow columns (orgKey table tenantUuid)

saveSettingsAuthentication :: WizardRequestContextC s m => U.UUID -> SettingsAuthentication -> m ()
saveSettingsAuthentication tenantUuid = saveSettingsRow columns (orgKey table tenantUuid)
