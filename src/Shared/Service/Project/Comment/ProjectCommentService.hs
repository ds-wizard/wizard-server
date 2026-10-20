module Shared.Service.Project.Comment.ProjectCommentService where

import Control.Monad (unless, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO, local)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Project.ProjectCommentDAO
import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Acl.Acl
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.Project.Comment.ProjectCommentList
import Shared.Model.Project.Comment.ProjectCommentThreadAssigned
import Shared.Model.Project.Comment.ProjectCommentThreadNotification
import Shared.Model.Project.Project
import Shared.Model.User.UserGroupMembership
import Shared.Model.User.UserSimple
import Shared.Model.User.UserSuggestion
import Shared.Service.Mail.Mailer
import Shared.Service.Project.Comment.ProjectCommentMapper
import Shared.Service.Project.ProjectAcl
import Shared.Service.Project.ProjectUtil
import Shared.Util.List
import Shared.Util.String (splitOn)
import Shared.Util.Uuid

getProjectCommentThreadsPage :: WizardRequestContextC s m => Maybe String -> Maybe U.UUID -> Maybe Bool -> Pageable -> [Sort] -> m (Page ProjectCommentThreadAssigned)
getProjectCommentThreadsPage mQuery mProjectUuid resolved pageable sort = do
  findAssignedProjectCommentThreadsPage mQuery mProjectUuid resolved pageable sort

getProjectCommentsByProjectUuid :: WizardRequestContextC s m => U.UUID -> Maybe String -> Maybe Bool -> m (M.Map String [ProjectCommentThreadList])
getProjectCommentsByProjectUuid projectUuid mPath mResolved = do
  project <- findProjectByUuid projectUuid
  checkCommentPermissionToProject project.workspaceUuid project.visibility project.sharing project.permissions
  editor <- catchError (hasEditPermissionToProject project.workspaceUuid project.visibility project.sharing project.permissions) (\_ -> return False)
  threads <- findProjectCommentThreadsForProject project.uuid mPath mResolved editor
  return . toCommentThreadsMap $ threads

getProjectCommentThread :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ProjectCommentThreadList
getProjectCommentThread projectUuid threadUuid = do
  threads <- concat . M.elems <$> getProjectCommentsByProjectUuid projectUuid Nothing Nothing
  maybe (throwError . NotExistsError $ threadNotFound) return (L.find ((== threadUuid) . (.uuid)) threads)
  where
    threadNotFound = _ERROR_DATABASE__ENTITY_NOT_FOUND "project_comment_thread" [("project_uuid", U.toString projectUuid), ("uuid", U.toString threadUuid)]

checkCommentChange :: WizardRequestContextC s m => ProjectCommentThreadList -> U.UUID -> m ()
checkCommentChange thread commentUuid = do
  comment <- maybe (throwError . NotExistsError $ commentNotFound) return (L.find ((== commentUuid) . (.uuid)) thread.comments)
  when thread.resolved (throwError . UserError $ _ERROR_SERVICE_PROJECT_COMMENT__THREAD_RESOLVED)
  checkAuthor comment.createdBy
  where
    commentNotFound = _ERROR_DATABASE__ENTITY_NOT_FOUND "project_comment" [("thread_uuid", U.toString thread.uuid), ("uuid", U.toString commentUuid)]

checkCommentThreadDeletion :: WizardRequestContextC s m => ProjectCommentThreadList -> m ()
checkCommentThreadDeletion thread = checkAuthor thread.createdBy

checkAuthor :: WizardRequestContextC s m => Maybe UserSuggestion -> m ()
checkAuthor mCreatedBy = do
  mCurrentUserUuid <- asks (fmap (.uuid) . (.currentUser'))
  unless (Maybe.isJust mCurrentUserUuid && fmap (.uuid) mCreatedBy == mCurrentUserUuid) $
    throwError . ForbiddenError $
      _ERROR_SERVICE_PROJECT_COMMENT__NOT_AUTHOR

checkCommentThreadAssignee :: WizardRequestContextC s m => U.UUID -> ProjectCommentThreadList -> U.UUID -> m ()
checkCommentThreadAssignee projectUuid thread userUuid = do
  project <- findProjectByUuid projectUuid
  unless (not thread.private && openForComments project) $ do
    groupUuids <- fmap (.userGroupUuid) <$> findUserGroupMembershipsByUserUuid userUuid
    let perm = if thread.private then _EDIT_PERM else _COMMENT_PERM
    unless (any (grantsTo userUuid groupUuids perm) project.permissions) $
      throwError . UserError $
        _ERROR_SERVICE_PROJECT_COMMENT__ASSIGNEE_WITHOUT_ACCESS (U.toString userUuid)

openForComments :: Project -> Bool
openForComments project =
  project.visibility `elem` [VisibleCommentProjectVisibility, VisibleEditProjectVisibility]
    || project.sharing `elem` [AnyoneWithLinkCommentProjectSharing, AnyoneWithLinkEditProjectSharing]

grantsTo :: U.UUID -> [U.UUID] -> String -> ProjectPerm -> Bool
grantsTo userUuid groupUuids perm projectPerm =
  perm `elem` projectPerm.perms
    && case projectPerm.memberType of
      UserProjectPermType -> projectPerm.memberUuid == userUuid
      UserGroupProjectPermType -> projectPerm.memberUuid `elem` groupUuids

duplicateCommentThreads :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
duplicateCommentThreads oldProjectUuid newProjectUuid = do
  threads <- findProjectCommentThreads oldProjectUuid
  traverse_ (duplicateCommentThread newProjectUuid) threads

duplicateCommentThread :: WizardRequestContextC s m => U.UUID -> ProjectCommentThread -> m ()
duplicateCommentThread newProjectUuid thread = do
  newUuid <- liftIO generateUuid
  let updatedCommentThread =
        thread
          { uuid = newUuid
          , projectUuid = newProjectUuid
          }
  insertProjectCommentThread updatedCommentThread
  traverse_ (duplicateComment newUuid) thread.comments

duplicateComment :: WizardRequestContextC s m => U.UUID -> ProjectComment -> m ()
duplicateComment newThreadUuid comment = do
  newUuid <- liftIO generateUuid
  let updatedComment =
        comment
          { uuid = newUuid
          , threadUuid = newThreadUuid
          }
  insertProjectComment updatedComment
  return ()

sendNotificationToNewAssignees :: WizardRequestContextC s m => m ()
sendNotificationToNewAssignees =
  runInTransaction $ do
    threads <- findProjectCommentThreadsForNotifying
    let threadGroups = groupBy (\t1 t2 -> t1.assignedTo.uuid == t2.assignedTo.uuid && t1.tenantUuid == t2.tenantUuid) threads
    traverse_ sendNotificationGroup threadGroups
    unsetProjectCommentThreadNotificationRequired

sendNotificationGroup :: WizardRequestContextC s m => [ProjectCommentThreadNotification] -> m ()
sendNotificationGroup [] = return ()
sendNotificationGroup notifications@(notification : _) =
  local (setTenantUuid notification.tenantUuid) $ do
    enriched <- traverse fillInQuestionTitle notifications
    sendProjectCommentThreadAssignedMail enriched

fillInQuestionTitle :: WizardRequestContextC s m => ProjectCommentThreadNotification -> m ProjectCommentThreadNotification
fillInQuestionTitle n = do
  title <-
    catchError
      ( do
          km <- compileProjectKnowledgeModel n.knowledgeModelPackageUuid n.selectedQuestionTagUuids
          return $ resolveQuestionTitleFromPath km n.path
      )
      (\_ -> return Nothing)
  return $ n {questionTitle = title}

resolveQuestionTitleFromPath :: KnowledgeModel -> String -> Maybe String
resolveQuestionTitleFromPath km path =
  let segments = Maybe.mapMaybe U.fromString (splitOn "." path)
      questions = Maybe.mapMaybe (`M.lookup` getQuestionsM km) segments
   in case reverse questions of
        q : _ -> Just (getTitle q)
        [] -> Nothing
