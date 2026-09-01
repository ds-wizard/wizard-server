module Shared.Service.Project.Comment.ProjectCommentService where

import Control.Monad.Except (catchError)
import Control.Monad.Reader (liftIO, local)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Data.UUID as U

import Shared.Database.DAO.Project.ProjectCommentDAO
import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.Project.Comment.ProjectCommentList
import Shared.Model.Project.Comment.ProjectCommentThreadAssigned
import Shared.Model.Project.Comment.ProjectCommentThreadNotification
import Shared.Model.Project.Project
import Shared.Model.User.UserSimple
import Shared.Service.KnowledgeModel.KnowledgeModelService
import Shared.Service.Mail.Mailer
import Shared.Service.Project.Comment.ProjectCommentMapper
import Shared.Service.Project.ProjectAcl
import Shared.Util.List
import Shared.Util.String (splitOn)
import Shared.Util.Uuid

getProjectCommentThreadsPage :: WizardRequestContextC s m => Maybe String -> Maybe U.UUID -> Maybe Bool -> Pageable -> [Sort] -> m (Page ProjectCommentThreadAssigned)
getProjectCommentThreadsPage mQuery mProjectUuid resolved pageable sort = do
  findAssignedProjectCommentThreadsPage mQuery mProjectUuid resolved pageable sort

getProjectCommentsByProjectUuid :: WizardRequestContextC s m => U.UUID -> Maybe String -> Maybe Bool -> m (M.Map String [ProjectCommentThreadList])
getProjectCommentsByProjectUuid projectUuid mPath mResolved = do
  project <- findProjectByUuid projectUuid
  checkCommentPermissionToProject project.visibility project.sharing project.permissions
  editor <- catchError (hasEditPermissionToProject project.visibility project.sharing project.permissions) (\_ -> return False)
  threads <- findProjectCommentThreadsForProject project.uuid mPath mResolved editor
  return . toCommentThreadsMap $ threads

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
sendNotificationGroup notifications@(notification : _) = do
  enriched <-
    local (setTenantUuid notification.tenantUuid) $
      traverse fillInQuestionTitle notifications
  sendProjectCommentThreadAssignedMail enriched

fillInQuestionTitle :: WizardRequestContextC s m => ProjectCommentThreadNotification -> m ProjectCommentThreadNotification
fillInQuestionTitle n = do
  title <-
    catchError
      ( do
          km <- compileKnowledgeModel [] (Just n.knowledgeModelPackageUuid) n.selectedQuestionTagUuids
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
