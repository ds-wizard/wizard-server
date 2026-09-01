module Shared.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.KnowledgeModel.Migration.KnowledgeModelMigration ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration

entityName = "knowledge_model_migration"

findKnowledgeModelMigrations :: WizardRequestContextC s m => m [KnowledgeModelMigration]
findKnowledgeModelMigrations = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findKnowledgeModelMigrationByEditorUuid :: WizardRequestContextC s m => U.UUID -> m KnowledgeModelMigration
findKnowledgeModelMigrationByEditorUuid editorUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString editorUuid)]

findKnowledgeModelMigrationByEditorUuid' :: WizardRequestContextC s m => U.UUID -> m (Maybe KnowledgeModelMigration)
findKnowledgeModelMigrationByEditorUuid' editorUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString editorUuid)]

insertKnowledgeModelMigration :: WizardRequestContextC s m => KnowledgeModelMigration -> m Int64
insertKnowledgeModelMigration = createInsertFn entityName

updateKnowledgeModelMigration :: WizardRequestContextC s m => KnowledgeModelMigration -> m Int64
updateKnowledgeModelMigration migration = do
  let sql =
        fromString
          "UPDATE knowledge_model_migration SET editor_uuid = ?, metamodel_version = ?, state = ?, editor_previous_package_uuid = ?, target_package_uuid = ?, editor_previous_package_events = ?, target_package_events = ?, result_events = ?, current_knowledge_model = ?, tenant_uuid = ?, created_at = ? WHERE tenant_uuid = ? AND editor_uuid = ?"
  let params = toRow migration ++ [toField migration.tenantUuid, toField migration.editorUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteKnowledgeModelMigrations :: WizardRequestContextC s m => m Int64
deleteKnowledgeModelMigrations = createDeleteEntitiesFn entityName

deleteKnowledgeModelMigrationByEditorUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteKnowledgeModelMigrationByEditorUuid editorUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString editorUuid)]
