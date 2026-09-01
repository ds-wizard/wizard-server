module Shared.Database.DAO.Project.ProjectCommentDAO where

import Control.Monad.Reader (asks, liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Project.Comment.ProjectComment ()
import Shared.Database.Mapping.Project.Comment.ProjectCommentThread ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Comment.ProjectComment
import Shared.Util.String

entityName = "project_comment"

insertProjectComment :: WizardRequestContextC s m => ProjectComment -> m Int64
insertProjectComment = createInsertFn entityName

insertProjectThreadAndComment :: WizardRequestContextC s m => ProjectCommentThread -> ProjectComment -> m Int64
insertProjectThreadAndComment thread comment = do
  let sql =
        fromString $
          f'
            "BEGIN TRANSACTION; \
            \INSERT INTO %s VALUES (%s); \
            \INSERT INTO %s VALUES (%s); \
            \COMMIT;"
            ["project_comment_thread", generateQuestionMarks' thread, entityName, generateQuestionMarks' comment]
  let params = toRow thread ++ toRow comment
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

insertProjectThreadAndComment' :: WizardRequestContextC s m => ProjectCommentThread -> ProjectComment -> m Int64
insertProjectThreadAndComment' thread comment = do
  let sql =
        fromString $
          f'
            "INSERT INTO %s VALUES (%s); \
            \INSERT INTO %s VALUES (%s); "
            ["project_comment_thread", generateQuestionMarks' thread, entityName, generateQuestionMarks' comment]
  let params = toRow thread ++ toRow comment
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

updateProjectCommentById :: WizardRequestContextC s m => ProjectComment -> m ProjectComment
updateProjectCommentById entity = do
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let updatedEntity = entity {updatedAt = now} :: ProjectComment
  let sql =
        fromString
          "UPDATE project_comment SET uuid = ?, text = ?, created_by = ?, created_at = ?, updated_at = ?, tenant_uuid = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow updatedEntity ++ [toField updatedEntity.uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedEntity

updateProjectCommentTextById :: WizardRequestContextC s m => U.UUID -> String -> m Int64
updateProjectCommentTextById uuid text = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE project_comment SET text = ?, updated_at = now() WHERE uuid = ? AND tenant_uuid = ?"
  let params = [toField text, toField uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteProjectComments :: WizardRequestContextC s m => m Int64
deleteProjectComments = createDeleteEntitiesFn entityName

deleteProjectCommentsByThreadUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteProjectCommentsByThreadUuid threadUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [("comment_thread_uuid", U.toString threadUuid), ("tenant_uuid", U.toString tenantUuid)]

deleteProjectCommentById :: WizardRequestContextC s m => U.UUID -> m Int64
deleteProjectCommentById uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [("uuid", U.toString uuid), ("tenant_uuid", U.toString tenantUuid)]
