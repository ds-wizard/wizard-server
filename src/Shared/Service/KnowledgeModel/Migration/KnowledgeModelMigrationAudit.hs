module Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationAudit where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Service.Audit.AuditService

auditKmMigrationCreate :: WizardRequestContextC s m => KnowledgeModelMigrationCreateDTO -> KnowledgeModelEditor -> m ()
auditKmMigrationCreate reqDto kmEditor =
  logAuditWithBody
    "knowledge_model_migration"
    "create"
    (U.toString kmEditor.uuid)
    ( M.fromList
        [("sourcePackageId", maybe "" U.toString kmEditor.previousPackageUuid), ("targetPackageUuid", U.toString reqDto.targetPackageUuid)]
    )

auditKmMigrationSolve :: WizardRequestContextC s m => U.UUID -> KnowledgeModelMigrationResolutionDTO -> m ()
auditKmMigrationSolve editorUuid reqDto =
  logAuditWithBody
    "knowledge_model_migration"
    "solve"
    (U.toString editorUuid)
    (M.fromList [("originalEventUuid", U.toString reqDto.originalEventUuid), ("action", show reqDto.action)])

auditKmMigrationApplyAll :: WizardRequestContextC s m => U.UUID -> m ()
auditKmMigrationApplyAll editorUuid = logAudit "knowledge_model_migration" "applyAll" (U.toString editorUuid)

auditKmMigrationCancel :: WizardRequestContextC s m => U.UUID -> m ()
auditKmMigrationCancel editorUuid = logAudit "knowledge_model_migration" "cancel" (U.toString editorUuid)

auditKmMigrationFinish :: WizardRequestContextC s m => U.UUID -> m ()
auditKmMigrationFinish editorUuid = logAudit "knowledge_model_migration" "finish" (U.toString editorUuid)
