module Shared.Database.DAO.Settings.SettingsSupportDAO where

import qualified Data.UUID as U

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsSupport ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_support"

childTables :: [String]
childTables = []

columns :: [String]
columns = ["support_email", "support_site_name", "support_site_url", "support_site_icon"]

findSettingsSupport :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m (Maybe SettingsSupport)
findSettingsSupport tenantUuid mWorkspaceUuid = findSettingsRow columns (scopedKey table tenantUuid mWorkspaceUuid)

saveSettingsSupport :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> SettingsSupport -> m ()
saveSettingsSupport tenantUuid mWorkspaceUuid = saveSettingsRow columns (scopedKey table tenantUuid mWorkspaceUuid)
