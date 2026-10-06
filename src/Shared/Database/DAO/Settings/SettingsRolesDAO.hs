module Shared.Database.DAO.Settings.SettingsRolesDAO where

import qualified Data.UUID as U

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsRoles ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_roles"

columns :: [String]
columns = ["default_role_uuid"]

findSettingsRoles :: WizardRequestContextC s m => U.UUID -> m (Maybe SettingsRoles)
findSettingsRoles tenantUuid = findSettingsRow columns (orgKey table tenantUuid)

saveSettingsRoles :: WizardRequestContextC s m => U.UUID -> SettingsRoles -> m ()
saveSettingsRoles tenantUuid = saveSettingsRow columns (orgKey table tenantUuid)
