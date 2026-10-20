module Shared.Service.KnowledgeModel.Editor.Event.EditorEventService where

import Control.Monad (replicateM)
import Control.Monad.Except (MonadError, liftEither, throwError)
import Control.Monad.Reader (asks, liftIO, local)
import Data.Aeson (toJSON)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsDTO
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Constant.KnowledgeModel
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Error.FieldError
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelUtil
import Shared.Model.KnowledgeModel.Tree.KnowledgeModelEntityNodeQuery
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Shared.Service.KnowledgeModel.Editor.EditorAcl
import Shared.Service.KnowledgeModel.Editor.EditorAuditLog
import Shared.Service.KnowledgeModel.Editor.EditorMapper
import Shared.Service.KnowledgeModel.Editor.EditorUtil
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventMapper
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventNames
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventResolution
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventValidation
import Shared.Service.KnowledgeModel.Squash.Squasher
import Shared.Service.KnowledgeModel.Tree.KnowledgeModelTreeMapper
import Shared.Util.Date (batchTimestamps, getCurrentTimeInMicroseconds)
import Shared.Util.List (lastSafe)
import Shared.Util.Logger
import Shared.Util.Uuid

createEditorEvents :: WizardRequestContextC s m => U.UUID -> KnowledgeModelEditorEventsChangeDTO -> m KnowledgeModelEditorEventsDTO
createEditorEvents editorUuid reqDto = do
  (resDto, newEvents) <- runInTransaction $ storeEditorEvents editorUuid reqDto
  broadcastKnowledgeModelEvents editorUuid newEvents
  return resDto

storeEditorEvents
  :: WizardRequestContextC s m
  => U.UUID
  -> KnowledgeModelEditorEventsChangeDTO
  -> m (KnowledgeModelEditorEventsDTO, [KnowledgeModelEvent])
storeEditorEvents editorUuid reqDto = do
  checkPermissionToEditor editorUuid
  validateBatchLimit knowledgeModelEditorEventBatchLimit reqDto.events
  editor <- findKnowledgeModelEditorByUuidLocked editorUuid
  validateNoMigration editorUuid
  storedEvents <- fmap (fmap toKnowledgeModelEvent) (findKnowledgeModelEventsByEditorUuid editorUuid)
  validateBaseRevision reqDto.baseRevision storedEvents
  let (baseEvents, addedEvents) = splitAtRevision reqDto.baseRevision storedEvents
  let names = createEventNames (maybe editorUuid (.uuid) (lastSafe baseEvents)) reqDto.events
  baseKm <- compileEditorKnowledgeModel editor baseEvents
  events <- createEvents baseEvents =<< resolveAtRevision baseKm names reqDto addedEvents
  km <- compileEditorKnowledgeModel editor (baseEvents ++ events)
  newEvents <- if null addedEvents && reqDto.dryRun /= Just True then insertEditorEvents editor events else return []
  let revision = fmap (.uuid) . lastSafe $ storedEvents ++ newEvents
  return (toEventsDTO revision names . toEntityNodes km outlineQuery . fmap (affectedEntityUuid km) $ events, newEvents)

compileEditorWithPendingEvents
  :: WizardRequestContextC s m
  => U.UUID
  -> [KnowledgeModelEditorEventChangeDTO]
  -> m (KnowledgeModelEditor, KnowledgeModel, KnowledgeModel, M.Map String U.UUID)
compileEditorWithPendingEvents editorUuid reqDtos = do
  checkPermissionToEditor editorUuid
  validateBatchLimit knowledgeModelEditorEventBatchLimit reqDtos
  editor <- findKnowledgeModelEditorByUuid editorUuid
  storedEvents <- fmap (fmap toKnowledgeModelEvent) (findKnowledgeModelEventsByEditorUuid editorUuid)
  let names = createEventNames (maybe editorUuid (.uuid) (lastSafe storedEvents)) reqDtos
  baseKm <- compileEditorKnowledgeModel editor storedEvents
  events <- resolveEditorEvents baseKm names reqDtos
  km <- compileEditorKnowledgeModel editor (storedEvents ++ events)
  return (editor, baseKm, km, names)

splitAtRevision :: Maybe U.UUID -> [KnowledgeModelEvent] -> ([KnowledgeModelEvent], [KnowledgeModelEvent])
splitAtRevision Nothing events = (events, [])
splitAtRevision (Just revision) events = maybe ([], events) (\index -> splitAt (index + 1) events) (L.findIndex ((== revision) . (.uuid)) events)

resolveAtRevision
  :: MonadError AppError m
  => KnowledgeModel
  -> M.Map String U.UUID
  -> KnowledgeModelEditorEventsChangeDTO
  -> [KnowledgeModelEvent]
  -> m [KnowledgeModelEvent]
resolveAtRevision km names reqDto addedEvents =
  case (resolveEditorEvents km names reqDto.events, addedEvents) of
    (eEvents, []) -> liftEither eEvents
    (Right events, _) | fmap toChange events == fmap toChange addedEvents -> return events
    _ -> throwError . ConflictError (_ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__OUTDATED_BASE_REVISION (maybe "" U.toString reqDto.baseRevision)) . toJSON $ addedEvents
  where
    toChange event = (event.parentUuid, event.entityUuid, event.content)

insertEditorEvents :: WizardRequestContextC s m => KnowledgeModelEditor -> [KnowledgeModelEvent] -> m [KnowledgeModelEvent]
insertEditorEvents editor events = do
  tenantUuid <- asks (.tenantUuid')
  traverse_ (insertKnowledgeModelEvent . toKnowledgeModelEditorEvent editor.uuid tenantUuid) events
  updateKnowledgeModelEditorSquashedByUuid editor.uuid False
  al_addKnowledgeModelEditorEvents editor (length events)
  return events

createEvents :: WizardRequestContextC s m => [KnowledgeModelEvent] -> [KnowledgeModelEvent] -> m [KnowledgeModelEvent]
createEvents storedEvents events = do
  now <- liftIO getCurrentTimeInMicroseconds
  uuids <- liftIO $ replicateM (length events) generateUuid
  let start = maybe now (max now . addUTCTime 0.000001 . (.createdAt)) (lastSafe storedEvents)
  return $ zipWith3 withEventIdentity uuids (batchTimestamps start) events

outlineQuery :: KnowledgeModelEntityNodeQuery
outlineQuery = KnowledgeModelEntityNodeQuery {depth = Nothing, content = False}

affectedEntityUuid :: KnowledgeModel -> KnowledgeModelEvent -> U.UUID
affectedEntityUuid km event
  | entityExists km event.entityUuid = event.entityUuid
  | otherwise = event.parentUuid

squashEvents :: WizardRequestContextC s m => m ()
squashEvents = do
  editors <- findEditorsForSquashing
  traverse_ (\(editorUuid, tenantUuid) -> local (setTenantUuid tenantUuid) (squashEventsForEditor editorUuid)) editors

squashEventsForEditor :: WizardRequestContextC s m => U.UUID -> m ()
squashEventsForEditor editorUuid =
  runInTransaction $ do
    logInfoI _CMP_SERVICE (f' "Squashing events for KM editor (editorUuid: '%s')" [U.toString editorUuid])
    tenantUuid <- asks (.tenantUuid')
    _ <- findKnowledgeModelEditorByUuidForSquashingLocked editorUuid
    kmEditorEvents <- findKnowledgeModelEventsByEditorUuid editorUuid
    let kmEvents = fmap toKnowledgeModelEvent kmEditorEvents
    let squashedEvents = squash kmEvents
    let squashedKmEditorEvents = fmap (toKnowledgeModelEditorEvent editorUuid tenantUuid) squashedEvents
    deleteKnowledgeModelEventsByEditorUuid editorUuid
    traverse_ insertKnowledgeModelEvent squashedKmEditorEvents
    updateKnowledgeModelEditorSquashedByUuid editorUuid True
    logInfoI
      _CMP_SERVICE
      ( f'
          "Squashing for KM editor '%s' finished successfully (before: %s, after %s)"
          [U.toString editorUuid, show . length $ kmEvents, show . length $ squashedEvents]
      )
