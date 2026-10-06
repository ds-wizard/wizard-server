module Shared.Database.DAO.Settings.SettingsLookAndFeelDAO where

import qualified Data.UUID as U

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsLookAndFeel ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_look_and_feel"

columns :: [String]
columns = ["app_title", "logo_url", "primary_color"]

findSettingsLookAndFeel :: WizardRequestContextC s m => U.UUID -> m (Maybe SettingsLookAndFeel)
findSettingsLookAndFeel tenantUuid = findSettingsRow columns (orgKey table tenantUuid)

saveSettingsLookAndFeel :: WizardRequestContextC s m => U.UUID -> SettingsLookAndFeel -> m ()
saveSettingsLookAndFeel tenantUuid = saveSettingsRow columns (orgKey table tenantUuid)
