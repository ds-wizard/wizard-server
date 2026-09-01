module Shared.Service.Project.Migration.ProjectMigrationAudit where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Project
import Shared.Service.Audit.AuditService

auditProjectMigration :: WizardRequestContextC s m => ProjectMigrationCreateDTO -> Project -> m ()
auditProjectMigration reqDto project =
  logAuditWithBody
    "project_migration"
    "migrate"
    (U.toString project.uuid)
    ( M.fromList
        [ ("sourceKnowledgeModelPackageUuid", U.toString project.knowledgeModelPackageUuid)
        , ("targetKnowledgeModelPackageUuid", U.toString reqDto.targetKnowledgeModelPackageUuid)
        ]
    )
