module Shared.Service.Project.Event.ProjectEventService where

import Control.Monad (unless, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (MonadIO, asks, liftIO, local)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Mcp.Project.McpRepliesChangeDTO
import Shared.Api.Resource.Mcp.Project.McpRepliesChangeResultDTO
import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.Project.Event.ProjectEventDTO
import Shared.Api.Resource.Project.ProjectContentChangeDTO
import Shared.Constant.Project
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Common.Lens
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.FieldError
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Websocket.WebsocketPerm
import Shared.Service.Project.Collaboration.ProjectCollaborationAcl
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.Project.Event.ProjectEventMapper
import Shared.Service.Project.Event.ProjectEventStorageService
import Shared.Service.Project.Event.ProjectEventValidation
import Shared.Service.Project.ProjectAcl
import Shared.Service.Project.ProjectAuditLog
import Shared.Service.Project.ProjectUtil
import Shared.Service.User.WizardUserMapper
import Shared.Util.Date (batchTimestamps, getCurrentTimeInMicroseconds)
import Shared.Util.List (dropWhileExclusive, groupBy, takeWhileInclusive)
import Shared.Util.Logger
import Shared.Util.Uuid

addEventToProject :: WizardRequestContextC s m => U.UUID -> ProjectEventChangeDTO -> m ()
addEventToProject projectUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkEditPermissionToProject project.workspaceUuid project.visibility project.sharing project.permissions
    mCurrentUser <- asks (.currentUser')
    let mCreatedBy = fmap toSuggestion' mCurrentUser
    addEvent project EditorWebsocketPerm mCreatedBy reqDto

addEventsToProject :: WizardRequestContextC s m => U.UUID -> ProjectContentChangeDTO -> m [ProjectEventDTO]
addEventsToProject projectUuid reqDto = do
  validateBatchLimit projectEventBatchLimit reqDto.events
  storeProjectEvents projectUuid (`validateProjectEventChangesWithValues` reqDto.events) reqDto.events

modifyProjectReplies :: WizardRequestContextC s m => U.UUID -> McpRepliesChangeDTO -> m McpRepliesChangeResultDTO
modifyProjectReplies projectUuid reqDto = do
  project <- findProjectByUuid projectUuid
  checkEditPermissionToProject project.workspaceUuid project.visibility project.sharing project.permissions
  current <- findCurrentReplies projectUuid
  (events, result) <- toRepliesChange current reqDto
  unless (null events) $ do
    km <- compileProjectKnowledgeModel project.knowledgeModelPackageUuid project.selectedQuestionTagUuids
    fileUuids <- findProjectFileUuidsFor projectUuid events
    validateReplies km fileUuids current events
    validateBatchLimit projectEventBatchLimit events
    void $ storeProjectEvents projectUuid (const (return ())) events
  return result

addCommentEventToProject :: WizardRequestContextC s m => U.UUID -> ProjectEventChangeDTO -> m ()
addCommentEventToProject projectUuid reqDto = do
  project <- findProjectByUuid projectUuid
  (_, perm, _) <- resolveProjectPermission projectUuid
  when (commentPrivacy reqDto == Just True) (checkEditPermission perm)
  validateNewCommentThread project reqDto
  mCreatedBy <- asks (fmap toSuggestion' . (.currentUser'))
  addEvent project perm mCreatedBy reqDto

checkFileReplyPath :: WizardRequestContextC s m => U.UUID -> String -> m U.UUID
checkFileReplyPath projectUuid path = do
  project <- findProjectByUuid projectUuid
  checkEditPermissionToProject project.workspaceUuid project.visibility project.sharing project.permissions
  km <- compileProjectKnowledgeModel project.knowledgeModelPackageUuid project.selectedQuestionTagUuids
  current <- findCurrentReplies projectUuid
  case findQuestionByPath km (Just current) path of
    Right (FileQuestion' question) -> return question.uuid
    Right _ -> throwError (fieldValidationError [] [("path", _ERROR_VALIDATION__PROJECT_EVENT_REPLY_TYPE_MISMATCH)])
    Left pathError -> throwError (fieldValidationError [] [("path", pathError)])

toRepliesChange :: MonadIO m => M.Map String ReplyValue -> McpRepliesChangeDTO -> m ([ProjectEventChangeDTO], McpRepliesChangeResultDTO)
toRepliesChange current reqDto = do
  events <- liftIO $ (++) <$> traverse clearReplyChange cleared <*> traverse setReplyChange changed
  return (events, McpRepliesChangeResultDTO (fmap fst changed) cleared)
  where
    cleared = L.nub (filter (\path -> M.member path current && M.notMember path reqDto.replies) reqDto.clear)
    changed = filter (\(path, value) -> M.lookup path current /= Just value) (orderReplies reqDto.replies)

storeProjectEvents :: WizardRequestContextC s m => U.UUID -> (Project -> m ()) -> [ProjectEventChangeDTO] -> m [ProjectEventDTO]
storeProjectEvents projectUuid validate changes = do
  mCreatedBy <- asks (fmap toSuggestion' . (.currentUser'))
  now <- liftIO getCurrentTimeInMicroseconds
  let events = zip (batchTimestamps now) changes
  runInTransaction $ do
    project <- findProjectByUuidLocked projectUuid
    checkEditPermissionToProject project.workspaceUuid project.visibility project.sharing project.permissions
    validate project
    traverse_ (uncurry (storeEvent projectUuid mCreatedBy)) events
    al_addProjectEvents project (length events)
  traverse_ (uncurry (broadcastEvent projectUuid mCreatedBy)) events
  return $ fmap (\(createdAt, event) -> toEventDTO' event mCreatedBy createdAt) events

clearReplyChange :: String -> IO ProjectEventChangeDTO
clearReplyChange path = (\uuid -> ClearReplyEventChangeDTO' (ClearReplyEventChangeDTO uuid path)) <$> generateUuid

setReplyChange :: (String, ReplyValue) -> IO ProjectEventChangeDTO
setReplyChange (path, value) = (\uuid -> SetReplyEventChangeDTO' (SetReplyEventChangeDTO uuid path value)) <$> generateUuid

squashProjectEvents :: WizardRequestContextC s m => m ()
squashProjectEvents = do
  projects <- findProjectForSquashing
  traverse_ (\(projectUuid, tenantUuid) -> local (setTenantUuid tenantUuid) (squashProjectEventsForProject projectUuid)) projects

squashProjectEventsForProject :: WizardRequestContextC s m => U.UUID -> m ()
squashProjectEventsForProject projectUuid =
  runInTransaction $ do
    logInfoI _CMP_SERVICE (f' "Squashing events for project (projectUuid: '%s')" [U.toString projectUuid])
    events <- findProjectEventsByProjectUuid projectUuid
    versions <- findProjectVersionsByProjectUuid projectUuid
    let squashedEvents = squash versions events
    syncProjectEventsWithDb events squashedEvents
    updateProjectSquashedByUuid projectUuid True
    logInfoI
      _CMP_SERVICE
      ( f'
          "Squashing for project '%s' finished successfully (before: %s, after %s)"
          [U.toString projectUuid, show . length $ events, show . length $ squashedEvents]
      )

instance Ord ProjectEvent where
  compare a b = compare (getCreatedAt a) (getCreatedAt b)

squash :: [ProjectVersion] -> [ProjectEvent] -> [ProjectEvent]
squash versions events =
  let groupedEvents = groupBy (\e1 e2 -> utctDay (getCreatedAt e1) == utctDay (getCreatedAt e2)) events
      squashedEvents = fmap squashOnePeriod (concatMap (splitAfterVersion versions) groupedEvents)
   in concat squashedEvents

splitAfterVersion :: [ProjectVersion] -> [ProjectEvent] -> [[ProjectEvent]]
splitAfterVersion _ [] = []
splitAfterVersion versions events =
  let isNotVersion event = not . L.any (\v -> v.eventUuid == getUuid event) $ versions
   in takeWhileInclusive isNotVersion events : splitAfterVersion versions (dropWhileExclusive isNotVersion events)

squashOnePeriod :: [ProjectEvent] -> [ProjectEvent]
squashOnePeriod = snd . foldr go (M.empty, [])
  where
    go
      :: ProjectEvent
      -> (M.Map String (Maybe U.UUID), [ProjectEvent])
      -> (M.Map String (Maybe U.UUID), [ProjectEvent])
    go event' (questions, events) =
      case event' of
        SetReplyEvent' event ->
          if Just event.createdBy == M.lookup event.path questions
            then (questions, events)
            else (M.insert event.path event.createdBy questions, event' : events)
        _ -> (questions, event' : events)
