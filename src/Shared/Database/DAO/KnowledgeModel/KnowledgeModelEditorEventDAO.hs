module Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorEvent ()
import Shared.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorRawEvent ()
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorRawEvent

entityName = "knowledge_model_editor_event"

findKnowledgeModelEventsByEditorUuid :: WizardRequestContextC s m => U.UUID -> m [KnowledgeModelEditorEvent]
findKnowledgeModelEventsByEditorUuid kmEditorUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString kmEditorUuid)] [Sort "created_at" Ascending]

findKnowledgeModelRawEventsByEditorUuid :: WizardRequestContextC s m => U.UUID -> m [KnowledgeModelEditorRawEvent]
findKnowledgeModelRawEventsByEditorUuid kmEditorUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString kmEditorUuid)] [Sort "created_at" Ascending]

insertKnowledgeModelEvent :: WizardRequestContextC s m => KnowledgeModelEditorEvent -> m Int64
insertKnowledgeModelEvent = createInsertFn entityName

insertKnowledgeModelRawEvent :: WizardRequestContextC s m => KnowledgeModelEditorRawEvent -> m Int64
insertKnowledgeModelRawEvent = createInsertFn entityName

deleteKnowledgeModelEventsByEditorUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteKnowledgeModelEventsByEditorUuid kmEditorUuid = createDeleteEntityByFn entityName [("editor_uuid", U.toString kmEditorUuid)]
