module Shared.Database.Mapping.Tenant.Config.TenantConfigProject where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Database.Mapping.Project.ProjectCreation ()
import Shared.Database.Mapping.Project.ProjectSharing ()
import Shared.Database.Mapping.Project.ProjectVisibility ()
import Shared.Model.Config.SimpleFeature
import Shared.Model.Tenant.Config.WizardTenantConfig

instance ToRow TenantConfigProject where
  toRow TenantConfigProject {..} =
    [ toField tenantUuid
    , toField projectVisibility.enabled
    , toField projectVisibility.defaultValue
    , toField projectSharing.enabled
    , toField projectSharing.defaultValue
    , toField projectSharing.anonymousEnabled
    , toField projectCreation
    , toField projectTagging.enabled
    , toField . PGArray $ projectTagging.tags
    , toField summaryReport.enabled
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow TenantConfigProject where
  fromRow = do
    tenantUuid <- field
    projectVisibilityEnabled <- field
    projectVisibilityDefaultValue <- field
    let projectVisibility =
          TenantConfigProjectVisibility
            { enabled = projectVisibilityEnabled
            , defaultValue = projectVisibilityDefaultValue
            }
    projectSharingEnabled <- field
    projectSharingDefaultValue <- field
    projectSharingAnonymousEnabled <- field
    let projectSharing =
          TenantConfigProjectSharing
            { enabled = projectSharingEnabled
            , defaultValue = projectSharingDefaultValue
            , anonymousEnabled = projectSharingAnonymousEnabled
            }
    projectCreation <- field
    projectTaggingEnabled <- field
    projectTaggingTags <- fromPGArray <$> field
    let projectTagging =
          TenantConfigProjectProjectTagging
            { enabled = projectTaggingEnabled
            , tags = projectTaggingTags
            }
    summaryReportEnabled <- field
    let summaryReport = SimpleFeature summaryReportEnabled
    createdAt <- field
    updatedAt <- field
    return TenantConfigProject {..}
