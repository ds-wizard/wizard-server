module Shared.Database.DAO.Project.ProjectCommentThreadDAO where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Project.Comment.ProjectCommentThread ()
import Shared.Database.Mapping.Project.Comment.ProjectCommentThreadAssigned ()
import Shared.Database.Mapping.Project.Comment.ProjectCommentThreadList ()
import Shared.Database.Mapping.Project.Comment.ProjectCommentThreadNotification ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.Project.Comment.ProjectCommentList
import Shared.Model.Project.Comment.ProjectCommentThreadAssigned
import Shared.Model.Project.Comment.ProjectCommentThreadNotification
import Shared.Util.Logger
import Shared.Util.Map (doubleGroupBy)
import Shared.Util.String
import Shared.Util.Uuid

entityName = "project_comment_thread"

findAssignedProjectCommentThreadsPage :: WizardRequestContextC s m => Maybe String -> Maybe U.UUID -> Maybe Bool -> Pageable -> [Sort] -> m (Page ProjectCommentThreadAssigned)
findAssignedProjectCommentThreadsPage mQuery mProjectUuid mResolved pageable sort = do
  -- 1. Prepare variables
  currentUser <- getCurrentUser
  tenantUuid <- asks (.tenantUuid')
  let (qCondition, qRegex) =
        case mQuery of
          Just query -> (" AND comment.text ~* ?", [regex query])
          Nothing -> ("", [])
  let (projectUuidCondition, projectUuidParam) =
        case mProjectUuid of
          Just projectUuid -> ("AND project.uuid = ?", [U.toString projectUuid])
          Nothing -> ("", [])
  let resolvedCondition =
        case mResolved of
          Just True -> "AND thread.resolved = true"
          Just False -> "AND thread.resolved = false"
          Nothing -> ""
  let params =
        [U.toString tenantUuid, U.toString currentUser.uuid]
          ++ qRegex
          ++ projectUuidParam
  let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
  -- 2. Get total count
  let countSql =
        fromString $
          f''
            "SELECT COUNT(DISTINCT thread.uuid) \
            \FROM project_comment_thread thread \
            \JOIN project ON project.uuid = thread.project_uuid AND project.tenant_uuid = thread.tenant_uuid \
            \LEFT JOIN project_comment comment ON comment.comment_thread_uuid = thread.uuid AND comment.tenant_uuid = thread.tenant_uuid \
            \WHERE thread.tenant_uuid = ? \
            \  AND thread.assigned_to = ? \
            \  AND comment.uuid = (SELECT comment.uuid \
            \                      FROM project_comment comment \
            \                      WHERE comment.comment_thread_uuid = thread.uuid AND comment.tenant_uuid = thread.tenant_uuid \
            \                      ORDER BY comment.created_at \
            \                      LIMIT 1) \
            \  ${qCondition} \
            \  ${projectUuidCondition} \
            \  ${resolvedCondition}"
            [ ("qCondition", qCondition)
            , ("projectUuidCondition", projectUuidCondition)
            , ("resolvedCondition", resolvedCondition)
            ]
  logQuery countSql params
  let action conn = query conn countSql params
  result <- runDB action
  let count =
        case result of
          [count] -> fromOnly count
          _ -> 0
  -- 3. Get entities
  let sql =
        fromString $
          f''
            "SELECT project.uuid, \
            \       project.name, \
            \       thread.uuid, \
            \       thread.path, \
            \       thread.resolved, \
            \       thread.private, \
            \       thread.updated_at, \
            \       comment.text AS comment_text, \
            \       u.uuid AS created_by_uuid, \
            \       u.first_name AS created_by_first_name, \
            \       u.last_name AS created_by_last_name, \
            \       u.email AS created_by_email, \
            \       u.image_url AS created_by_image_url, \
            \       u.affiliation AS created_by_affiliation \
            \FROM project_comment_thread thread \
            \JOIN project ON project.uuid = thread.project_uuid AND project.tenant_uuid = thread.tenant_uuid \
            \LEFT JOIN user_entity u ON u.uuid = thread.created_by AND u.tenant_uuid = thread.tenant_uuid \
            \LEFT JOIN project_comment comment ON comment.comment_thread_uuid = thread.uuid AND comment.tenant_uuid = thread.tenant_uuid \
            \WHERE thread.tenant_uuid = ? \
            \  AND thread.assigned_to = ? \
            \  AND comment.uuid = (SELECT comment.uuid \
            \                      FROM project_comment comment \
            \                      WHERE comment.comment_thread_uuid = thread.uuid AND comment.tenant_uuid = thread.tenant_uuid \
            \                      ORDER BY comment.created_at \
            \                      LIMIT 1) \
            \  ${qCondition} \
            \  ${projectUuidCondition} \
            \  ${resolvedCondition} \
            \${sort} \
            \OFFSET ${offset} \
            \LIMIT ${limit}"
            [ ("qCondition", qCondition)
            , ("projectUuidCondition", projectUuidCondition)
            , ("resolvedCondition", resolvedCondition)
            , ("sort", mapSort sort)
            , ("offset", show skip)
            , ("limit", show sizeI)
            ]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  -- 4. Constructor response
  let metadata =
        PageMetadata
          { size = sizeI
          , totalElements = count
          , totalPages = computeTotalPage count sizeI
          , number = pageI
          }
  return $ Page "projectCommentThreads" metadata entities

findProjectCommentThreads :: WizardRequestContextC s m => U.UUID -> m [ProjectCommentThread]
findProjectCommentThreads projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT project_comment_thread.*, \
          \       (SELECT array_agg(concat(uuid, ':::::', text, ':::::', comment_thread_uuid, ':::::', tenant_uuid, ':::::', created_by, ':::::', created_at, ':::::', updated_at)) AS comments \
          \        FROM project_comment \
          \        WHERE project_comment.comment_thread_uuid = project_comment_thread.uuid \
          \        GROUP BY comment_thread_uuid) AS comments \
          \FROM project_comment_thread \
          \WHERE project_uuid = ? AND tenant_uuid = ?"
  let params = [toField projectUuid, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findProjectCommentThreadsForProject :: WizardRequestContextC s m => U.UUID -> Maybe String -> Maybe Bool -> Bool -> m [ProjectCommentThreadList]
findProjectCommentThreadsForProject projectUuid mPath mResolved editor = do
  tenantUuid <- asks (.tenantUuid')
  let (pathCondition, pathParam) =
        case mPath of
          Just path -> ("AND path = ?", [toField path])
          Nothing -> ("", [])
  let resolvedCondition =
        case mResolved of
          Just True -> "AND resolved = true"
          Just False -> "AND resolved = false"
          Nothing -> ""
  let privateCondition =
        if editor
          then ""
          else "AND private = false"
  let sql =
        fromString $
          f''
            "SELECT thread.uuid, \
            \       thread.path, \
            \       thread.resolved, \
            \       thread.private, \
            \       thread.created_at, \
            \       thread.updated_at, \
            \       assigned_to_user.uuid, \
            \       assigned_to_user.first_name, \
            \       assigned_to_user.last_name, \
            \       assigned_to_user.email, \
            \       assigned_to_user.image_url, \
            \       assigned_to_user.affiliation, \
            \       created_by_user.uuid, \
            \       created_by_user.first_name, \
            \       created_by_user.last_name, \
            \       created_by_user.email, \
            \       created_by_user.image_url, \
            \       created_by_user.affiliation, \
            \       (SELECT array_agg(concat(comment.uuid, '<:::::>', \
            \                                comment.text, '<:::::>', \
            \                                comment.created_at, '<:::::>', \
            \                                comment.updated_at, '<:::::>', \
            \                                user_entity.uuid, '<:::::>', \
            \                                user_entity.first_name, '<:::::>', \
            \                                user_entity.last_name, '<:::::>', \
            \                                user_entity.email, '<:::::>', \
            \                                user_entity.image_url, '<:::::>', \
            \                                user_entity.affiliation \
            \                        )) AS comments \
            \         FROM project_comment comment \
            \         LEFT JOIN user_entity ON user_entity.uuid = comment.created_by \
            \         WHERE comment.comment_thread_uuid = thread.uuid \
            \         GROUP BY comment_thread_uuid) AS comments \
            \FROM project_comment_thread thread \
            \LEFT JOIN user_entity assigned_to_user ON assigned_to_user.uuid = thread.assigned_to \
            \LEFT JOIN user_entity created_by_user ON created_by_user.uuid = thread.created_by \
            \WHERE thread.project_uuid = ? \
            \  AND thread.tenant_uuid = ? \
            \  ${pathCondition} \
            \  ${resolvedCondition} \
            \  ${privateCondition}"
            [ ("pathCondition", pathCondition)
            , ("resolvedCondition", resolvedCondition)
            , ("privateCondition", privateCondition)
            ]
  let params = toField projectUuid : toField tenantUuid : pathParam
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findProjectCommentThreadsSimple :: WizardRequestContextC s m => U.UUID -> Bool -> Bool -> m (M.Map String (M.Map U.UUID Int))
findProjectCommentThreadsSimple projectUuid resolved editor = do
  tenantUuid <- asks (.tenantUuid')
  let privateCondition =
        if editor
          then ""
          else "AND project_comment_thread.private = false"
  let sql =
        fromString $
          f''
            "SELECT path, project_comment_thread.uuid::text, count(project_comment.uuid)::text \
            \FROM project_comment_thread \
            \LEFT JOIN project_comment ON project_comment_thread.uuid = project_comment.comment_thread_uuid AND project_comment_thread.tenant_uuid = project_comment.tenant_uuid \
            \WHERE project_comment_thread.project_uuid = ? \
            \  AND project_comment_thread.tenant_uuid = ? \
            \  AND project_comment_thread.resolved = ? \
            \  ${privateCondition} \
            \GROUP BY path, project_comment_thread.uuid"
            [("privateCondition", privateCondition)]
  let params = [toField projectUuid, toField tenantUuid, toField resolved]
  logQuery sql params
  let action conn = query conn sql params
  results <- runDB action
  return $ doubleGroupBy id u' read results

findProjectCommentThreadsForNotifying :: WizardRequestContextC s m => m [ProjectCommentThreadNotification]
findProjectCommentThreadsForNotifying = do
  let sql =
        "SELECT project.uuid, \
        \       project.name, \
        \       project.knowledge_model_package_uuid, \
        \       project.selected_question_tag_uuids, \
        \       project.tenant_uuid, \
        \       thread.uuid, \
        \       thread.path, \
        \       thread.resolved, \
        \       thread.private, \
        \       assigned_to.uuid, \
        \       assigned_to.first_name, \
        \       assigned_to.last_name, \
        \       assigned_to.email, \
        \       assigned_by.uuid, \
        \       assigned_by.first_name, \
        \       assigned_by.last_name, \
        \       assigned_by.email, \
        \       (SELECT comment.text \
        \        FROM project_comment comment \
        \        WHERE comment.comment_thread_uuid = thread.uuid \
        \          AND comment.tenant_uuid = thread.tenant_uuid \
        \        ORDER BY comment.created_at \
        \        LIMIT 1) comment_text, \
        \        tenant.client_url, \
        \        config_look_and_feel.app_title AS app_title, \
        \        config_look_and_feel.logo_url AS logo_url, \
        \        config_look_and_feel.primary_color AS primary_color, \
        \        config_look_and_feel.illustrations_color AS illustrations_color, \
        \        config_privacy_and_support.support_email AS support_email, \
        \        config_mail.config_uuid AS mail_config_uuid \
        \FROM project_comment_thread thread \
        \JOIN project ON project.uuid = thread.project_uuid AND project.tenant_uuid = thread.tenant_uuid \
        \JOIN user_entity assigned_to ON assigned_to.uuid = thread.assigned_to AND assigned_to.tenant_uuid = thread.tenant_uuid \
        \LEFT JOIN user_entity assigned_by ON assigned_by.uuid = thread.assigned_by AND assigned_by.tenant_uuid = thread.tenant_uuid \
        \JOIN tenant ON tenant.uuid = thread.tenant_uuid \
        \JOIN config_look_and_feel ON config_look_and_feel.tenant_uuid = thread.tenant_uuid \
        \JOIN config_privacy_and_support ON config_privacy_and_support.tenant_uuid = thread.tenant_uuid \
        \JOIN config_mail ON config_mail.tenant_uuid = thread.tenant_uuid \
        \WHERE thread.notification_required = true"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findProjectCommentThreadById :: WizardRequestContextC s m => U.UUID -> m (Maybe ProjectCommentThread)
findProjectCommentThreadById uuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT project_comment_thread.*, \
          \       (SELECT array_agg(concat(uuid, ':::::', text, ':::::', comment_thread_uuid, ':::::', tenant_uuid, ':::::', created_by, ':::::', created_at, ':::::', updated_at)) AS comments \
          \        FROM project_comment \
          \        WHERE project_comment.comment_thread_uuid = project_comment_thread.uuid \
          \        GROUP BY comment_thread_uuid) AS comments \
          \FROM project_comment_thread \
          \WHERE uuid = ? AND tenant_uuid = ?"
  let params = [toField uuid, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  case entities of
    [] -> return Nothing
    [entity] -> return . Just $ entity
    _ ->
      throwError $
        GeneralServerError
          ( f'
              "createFindEntityByFn: find more entities found than one (entity: %s, param: %s)"
              [entityName, show [("uuid", uuid)]]
          )

insertProjectCommentThread :: WizardRequestContextC s m => ProjectCommentThread -> m Int64
insertProjectCommentThread = createInsertFn entityName

updateProjectCommentThreadById :: WizardRequestContextC s m => ProjectCommentThread -> m ProjectCommentThread
updateProjectCommentThreadById entity = do
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let updatedEntity = entity {updatedAt = now} :: ProjectCommentThread
  let sql =
        fromString
          "UPDATE project_comment_thread SET uuid = ?, text = ?, project_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, tenant_uuid = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow updatedEntity ++ [toField updatedEntity.uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedEntity

updateProjectCommentThreadResolvedById :: WizardRequestContextC s m => U.UUID -> Bool -> m Int64
updateProjectCommentThreadResolvedById uuid resolved = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE project_comment_thread SET resolved = ?, updated_at = now() WHERE uuid = ? AND tenant_uuid = ?"
  let params = [toField resolved, toField uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateProjectCommentThreadAssignee :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> Maybe U.UUID -> m Int64
updateProjectCommentThreadAssignee uuid assignedTo assignedBy = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE project_comment_thread SET assigned_to = ?, assigned_by = ?, notification_required = true, updated_at = now() WHERE uuid = ? AND tenant_uuid = ?"
  let params = [toField assignedTo, toField assignedBy, toField uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

unsetProjectCommentThreadNotificationRequired :: WizardRequestContextC s m => m ()
unsetProjectCommentThreadNotificationRequired = do
  let sql = "UPDATE project_comment_thread SET notification_required = false WHERE notification_required = true"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
  return ()

deleteProjectCommentThreads :: WizardRequestContextC s m => m Int64
deleteProjectCommentThreads = createDeleteEntitiesFn entityName

deleteProjectCommentThreadById :: WizardRequestContextC s m => U.UUID -> m Int64
deleteProjectCommentThreadById uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [("uuid", U.toString uuid), ("tenant_uuid", U.toString tenantUuid)]
