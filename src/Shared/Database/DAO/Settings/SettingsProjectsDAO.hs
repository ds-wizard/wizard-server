module Shared.Database.DAO.Settings.SettingsProjectsDAO where

import qualified Data.UUID as U

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsProjects ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_projects"

childTables :: [String]
childTables = []

columns :: [String]
columns = ["visibility_enabled", "visibility_default_value", "sharing_enabled", "sharing_default_value", "sharing_anonymous_enabled", "creation", "project_tagging_enabled", "project_tagging_tags", "summary_report"]

findSettingsProjects :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m (Maybe SettingsProjects)
findSettingsProjects tenantUuid mWorkspaceUuid = findSettingsRow columns (scopedKey table tenantUuid mWorkspaceUuid)

saveSettingsProjects :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> SettingsProjects -> m ()
saveSettingsProjects tenantUuid mWorkspaceUuid = saveSettingsRow columns (scopedKey table tenantUuid mWorkspaceUuid)
