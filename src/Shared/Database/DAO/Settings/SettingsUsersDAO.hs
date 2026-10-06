module Shared.Database.DAO.Settings.SettingsUsersDAO where

import qualified Data.UUID as U

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsUsers ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_users"

columns :: [String]
columns = ["affiliations"]

findSettingsUsers :: WizardRequestContextC s m => U.UUID -> m (Maybe SettingsUsers)
findSettingsUsers tenantUuid = findSettingsRow columns (orgKey table tenantUuid)

saveSettingsUsers :: WizardRequestContextC s m => U.UUID -> SettingsUsers -> m ()
saveSettingsUsers tenantUuid = saveSettingsRow columns (orgKey table tenantUuid)
