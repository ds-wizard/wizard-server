module Shared.Database.DAO.Settings.SettingsRegistryDAO where

import qualified Data.UUID as U

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsRegistry ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_registry"

columns :: [String]
columns = ["enabled", "api_key"]

findSettingsRegistry :: WizardRequestContextC s m => U.UUID -> m (Maybe SettingsRegistry)
findSettingsRegistry tenantUuid = findSettingsRow columns (orgKey table tenantUuid)

saveSettingsRegistry :: WizardRequestContextC s m => U.UUID -> SettingsRegistry -> m ()
saveSettingsRegistry tenantUuid = saveSettingsRow columns (orgKey table tenantUuid)
