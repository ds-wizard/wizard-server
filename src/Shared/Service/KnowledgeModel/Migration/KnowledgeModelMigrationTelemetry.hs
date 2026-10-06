module Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationTelemetry where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Shared.Model.Context.WizardRequestContext
import Shared.Service.Telemetry.TelemetryService

telemetryKmMigrationSolve :: WizardRequestContextC s m => U.UUID -> KnowledgeModelMigrationResolutionDTO -> m ()
telemetryKmMigrationSolve editorUuid reqDto =
  logTelemetryWithBody
    "knowledge_model_migration"
    "solve"
    (U.toString editorUuid)
    (M.fromList [("originalEventUuid", U.toString reqDto.originalEventUuid), ("action", show reqDto.action)])

telemetryKmMigrationApplyAll :: WizardRequestContextC s m => U.UUID -> m ()
telemetryKmMigrationApplyAll editorUuid = logTelemetry "knowledge_model_migration" "applyAll" (U.toString editorUuid)
