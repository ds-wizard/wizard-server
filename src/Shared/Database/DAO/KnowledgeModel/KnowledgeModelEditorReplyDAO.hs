module Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO where

import Control.Monad.Reader (asks)
import qualified Data.List as L
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorReply ()
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorReply
import Shared.Util.Logger

entityName = "knowledge_model_editor_reply"

findKnowledgeModelRepliesByEditorUuid :: WizardRequestContextC s m => U.UUID -> m [KnowledgeModelEditorReply]
findKnowledgeModelRepliesByEditorUuid kmEditorUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString kmEditorUuid)] [Sort "created_at" Descending]

insertKnowledgeModelReply :: WizardRequestContextC s m => KnowledgeModelEditorReply -> m Int64
insertKnowledgeModelReply = createInsertFn entityName

updateKnowledgeModelRepliesByEditorUuid :: WizardRequestContextC s m => U.UUID -> [KnowledgeModelEditorReply] -> m Int64
updateKnowledgeModelRepliesByEditorUuid editorUuid editorReplies = do
  case editorReplies of
    [] -> do
      tenantUuid <- asks (.tenantUuid')
      createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString editorUuid)]
    (editorReply : _) -> do
      tenantUuid <- asks (.tenantUuid')
      let insertSql = L.intercalate "," . fmap (const "(?, ?, ?, ?, ?, ?, ?, ?)") $ editorReplies
      let sql =
            fromString $
              f'
                "BEGIN TRANSACTION; \
                \DELETE FROM knowledge_model_editor_reply WHERE tenant_uuid = ? AND editor_uuid = ?; \
                \INSERT INTO knowledge_model_editor_reply (path, value_type, value, value_raw, editor_uuid, created_by, tenant_uuid, created_at) \
                \VALUES %s; \
                \COMMIT;"
                [insertSql]
      let params =
            [toField editorReply.tenantUuid, toField editorReply.knowledgeModelEditorUuid]
              ++ concatMap toRow editorReplies
      logInsertAndUpdate sql params
      let action conn = execute conn sql params
      runDB action

deleteKnowledgeModelRepliesByEditorUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteKnowledgeModelRepliesByEditorUuid kmEditorUuid = createDeleteEntityByFn entityName [("editor_uuid", U.toString kmEditorUuid)]
