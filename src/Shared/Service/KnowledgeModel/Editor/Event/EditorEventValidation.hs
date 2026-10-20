module Shared.Service.KnowledgeModel.Editor.Event.EditorEventValidation where

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError, throwError)
import Data.Either (fromRight)
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Error.FieldError
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Localization.LocaleRecord
import Shared.Service.KnowledgeModel.Compiler.Compiler
import Shared.Service.KnowledgeModel.Editor.EditorUtil
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventReferenceValidation
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventStructureValidation
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventValueValidation

validateWebsocketEditorEvent :: WizardRequestContextC s m => U.UUID -> KnowledgeModelEvent -> m ()
validateWebsocketEditorEvent editorUuid event = do
  (_, km) <- findEditorKnowledgeModel editorUuid []
  let errors = validateEventStructureAndReferences km (applyEvent km event) event
  unless (null errors) $
    throwError (fieldValidationError [_ERROR_VALIDATION__KM_EVENT_REJECTED (U.toString event.uuid)] errors)

validateEvent :: KnowledgeModel -> KnowledgeModel -> KnowledgeModelEvent -> [(String, LocaleRecord)]
validateEvent km nextKm event = validateEventStructureAndReferences km nextKm event ++ validateEventValues km nextKm event

validateEventStructureAndReferences :: KnowledgeModel -> KnowledgeModel -> KnowledgeModelEvent -> [(String, LocaleRecord)]
validateEventStructureAndReferences km nextKm event = validateEventStructure km event ++ validateEventReferences km nextKm event

applyEvent :: KnowledgeModel -> KnowledgeModelEvent -> KnowledgeModel
applyEvent km event = fromRight km (compile (Just km) [event])

validateNoMigration :: WizardRequestContextC s m => U.UUID -> m ()
validateNoMigration editorUuid = do
  mMigration <- findKnowledgeModelMigrationByEditorUuid' editorUuid
  when (isJust mMigration) (throwError . UserError $ _ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__MIGRATION_IN_PROGRESS)

validateBaseRevision :: MonadError AppError m => Maybe U.UUID -> [KnowledgeModelEvent] -> m ()
validateBaseRevision (Just revision) events
  | all ((/= revision) . (.uuid)) events = throwError . UserError $ _ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__UNKNOWN_BASE_REVISION (U.toString revision)
validateBaseRevision _ _ = return ()
