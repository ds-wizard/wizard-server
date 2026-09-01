module Shared.Service.KnowledgeModel.Metamodel.Migrator.KnowledgeModelEditorMigrator (
  migrateAll,
) where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import qualified Data.Aeson as A
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import qualified Data.Vector as Vector

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelRawEventJM ()
import Shared.Constant.KnowledgeModel
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Service.KnowledgeModel.Editor.EditorMapper
import Shared.Service.KnowledgeModel.Metamodel.Migrator.CommonDB

migrateAll :: WizardRequestContextC s m => m ()
migrateAll = do
  logMigrationStarted "knowledge_model_Editor"
  kmPkgs <- findEditorsByUnsupportedMetamodelVersion knowledgeModelMetamodelVersion
  tenantUuid <- asks (.tenantUuid')
  traverse_ (migrateOneInDB tenantUuid) kmPkgs
  logMigrationCompleted "knowledge_model_Editor"

-- --------------------------------
-- PRIVATE
-- --------------------------------
migrateOneInDB :: WizardRequestContextC s m => U.UUID -> KnowledgeModelEditor -> m ()
migrateOneInDB tenantUuid editor = do
  kmEvents <- findKnowledgeModelRawEventsByEditorUuid editor.uuid
  let events = A.Array . Vector.fromList . fmap (A.toJSON . toKnowledgeModelRawEvent) $ kmEvents
  migrateEventField "knowledge_model_editor" editor.createdAt editor.metamodelVersion events $ \eventsMigratedValue -> do
    case A.fromJSON eventsMigratedValue of
      A.Error error -> logMigrationFailedToConvertToNewMetamodelVersion "knowledge_model_editor" error
      A.Success eventsMigrated -> do
        deleteKnowledgeModelEventsByEditorUuid editor.uuid
        let kmEventsMigrated = fmap (toKnowledgeModelEditorRawEvent editor.uuid tenantUuid) eventsMigrated
        traverse_ insertKnowledgeModelRawEvent kmEventsMigrated
        void $ updateKnowledgeModelEditorMetamodelVersion editor.uuid knowledgeModelMetamodelVersion
