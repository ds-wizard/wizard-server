module Shared.Database.Mapping.Settings.SettingsProjects where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Database.Mapping.Common
import Shared.Database.Mapping.Project.ProjectSharing ()
import Shared.Database.Mapping.Project.ProjectVisibility ()
import Shared.Model.Config.SimpleFeature
import Shared.Model.Settings.Settings

instance ToField ProjectCreation where
  toField = toFieldGenericEnum

instance FromField ProjectCreation where
  fromField = fromFieldGenericEnum

instance ToRow SettingsProjects where
  toRow SettingsProjects {..} =
    [ toField projectVisibility.enabled
    , toField projectVisibility.defaultValue
    , toField projectSharing.enabled
    , toField projectSharing.defaultValue
    , toField projectSharing.anonymousEnabled
    , toField projectCreation
    , toField projectTagging.enabled
    , toField . PGArray $ projectTagging.tags
    , toField summaryReport.enabled
    ]

instance FromRow SettingsProjects where
  fromRow = do
    projectVisibility <- SettingsProjectsVisibility <$> field <*> field
    projectSharing <- SettingsProjectsSharing <$> field <*> field <*> field
    projectCreation <- field
    projectTagging <- SettingsProjectsTagging <$> field <*> (fromPGArray <$> field)
    summaryReport <- SimpleFeature <$> field
    return SettingsProjects {..}
