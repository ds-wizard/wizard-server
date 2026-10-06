module Shared.Database.DAO.Settings.SettingsFeaturesDAO where

import qualified Data.UUID as U

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsFeatures ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_features"

columns :: [String]
columns = ["ai_assistant_enabled", "tours_enabled"]

findSettingsFeatures :: WizardRequestContextC s m => U.UUID -> m (Maybe SettingsFeatures)
findSettingsFeatures tenantUuid = findSettingsRow columns (orgKey table tenantUuid)

saveSettingsFeatures :: WizardRequestContextC s m => U.UUID -> SettingsFeatures -> m ()
saveSettingsFeatures tenantUuid = saveSettingsRow columns (orgKey table tenantUuid)
