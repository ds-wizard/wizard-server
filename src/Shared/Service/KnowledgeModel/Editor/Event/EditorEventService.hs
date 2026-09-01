module Shared.Service.KnowledgeModel.Editor.Event.EditorEventService where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Service.KnowledgeModel.Editor.EditorMapper
import Shared.Service.KnowledgeModel.Squash.Squasher
import Shared.Util.Logger

squashEvents :: WizardRequestContextC s m => m ()
squashEvents = do
  editorUuids <- findEditorsForSquashing
  traverse_ squashEventsForEditor editorUuids

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
    logInfoI
      _CMP_SERVICE
      ( f'
          "Squashing for KM editor '%s' finished successfully (before: %s, after %s)"
          [U.toString editorUuid, show . length $ kmEvents, show . length $ squashedEvents]
      )
