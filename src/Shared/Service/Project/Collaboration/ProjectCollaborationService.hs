module Shared.Service.Project.Collaboration.ProjectCollaborationService where

import Control.Monad (when)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Data.Time
import qualified Data.UUID as U
import Network.WebSockets (Connection)

import Shared.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.Websocket.ProjectMessageJM ()
import Shared.Api.Resource.Websocket.WebsocketActionJM ()
import Shared.Cache.ProjectWebsocketCache
import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.Project.ProjectCommentDAO
import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Integration.Aws.Lambda
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Model.Project.Project
import Shared.Model.User.OnlineUserInfo
import Shared.Model.User.UserGroupMembership
import Shared.Model.User.UserSuggestion
import Shared.Model.Websocket.WebsocketMessage
import Shared.Model.Websocket.WebsocketRecord
import Shared.Service.Project.Collaboration.ProjectCollaborationAcl
import Shared.Service.Project.Collaboration.ProjectCollaborationMapper
import Shared.Service.Project.Comment.ProjectCommentMapper
import Shared.Service.Project.Event.ProjectEventMapper
import Shared.Service.Websocket.WebsocketService
import Shared.Util.Uuid
import Shared.Util.Websocket

putUserOnline :: WizardRequestContextC s m => U.UUID -> U.UUID -> Connection -> m ()
putUserOnline projectUuid connectionUuid connection = do
  myself <- createProjectRecord connectionUuid connection projectUuid
  checkViewPermission myself.entityPerm
  addToCache myself
  logWS connectionUuid "New user added to the list"
  setUserList projectUuid connectionUuid

deleteUser :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
deleteUser projectUuid connectionUuid = do
  deleteFromCache connectionUuid
  setUserList projectUuid connectionUuid

setUserList :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
setUserList projectUuid connectionUuid = do
  logWS connectionUuid "Informing other users about user list changes"
  records <- getAllFromCache
  broadcast (U.toString projectUuid) records (toSetUserListMessage records) disconnectUser
  logWS connectionUuid "Informed completed"

updatePermsForOnlineUsers :: WizardRequestContextC s m => U.UUID -> ProjectVisibility -> ProjectSharing -> [ProjectPerm] -> m ()
updatePermsForOnlineUsers projectUuid visibility sharing permissions = do
  currentTenantUuid <- asks (.tenantUuid')
  serverConfig <- asks (.serverConfig')
  if isJust serverConfig.cloud.signalBridgeUrl
    then do
      let dto = AKM.fromList [("projectUuid", U.toString projectUuid), ("tenantUuid", U.toString currentTenantUuid)]
      invokeLambda serverConfig.signalBridge.updatePermsArn (BSL.toStrict . A.encode $ dto)
      return ()
    else do
      records <- getAllFromCache
      traverse_ updatePerm records
  where
    updatePerm record =
      when
        (record.entityId == U.toString projectUuid)
        ( do
            let permission =
                  case record.user of
                    user@LoggedOnlineUserInfo {uuid = uuid, role = userRole, groupUuids = groupUuids} ->
                      getPermission visibility sharing permissions (Just uuid) userRole.permissions groupUuids
                    user@AnonymousOnlineUserInfo {..} ->
                      getPermission visibility sharing permissions Nothing [] []
            let updatedRecord = record {entityPerm = permission}
            updateCache updatedRecord
            disconnectUserIfLostPermission updatedRecord
        )

removeUserGroupFromUsers :: WizardRequestContextC s m => U.UUID -> [U.UUID] -> m ()
removeUserGroupFromUsers userGroupUuid userUuids = do
  currentTenantUuid <- asks (.tenantUuid')
  serverConfig <- asks (.serverConfig')
  if isJust serverConfig.cloud.signalBridgeUrl
    then do
      let dto = AKM.fromList [("userGroupUuid", U.toString userGroupUuid), ("tenantUuid", U.toString currentTenantUuid)]
      invokeLambda serverConfig.signalBridge.updateUserGroupArn (BSL.toStrict . A.encode $ dto)
      return ()
    else do
      records <- getAllFromCache
      traverse_ updatePerm records
  where
    updatePerm record =
      case record.user of
        user@LoggedOnlineUserInfo {uuid = uuid, groupUuids = groupUuids} -> do
          when
            (user.uuid `elem` userUuids)
            ( do
                let updatedRecord = record {user = user {groupUuids = filter (/= userGroupUuid) user.groupUuids}}
                updateCache updatedRecord
            )
        user@AnonymousOnlineUserInfo {..} -> return ()

setProject :: WizardRequestContextC s m => U.UUID -> ProjectDetailWsDTO -> m ()
setProject projectUuid reqDto = do
  currentTenantUuid <- asks (.tenantUuid')
  serverConfig <- asks (.serverConfig')
  if isJust serverConfig.cloud.signalBridgeUrl
    then do
      let dto =
            AKM.fromList
              [ ("projectUuid", A.String . U.toText $ projectUuid)
              , ("tenantUuid", A.String . U.toText $ currentTenantUuid)
              , ("message", A.toJSON reqDto)
              ]
      invokeLambda serverConfig.signalBridge.setProjectArn (BSL.toStrict . A.encode $ dto)
      return ()
    else do
      logWS U.nil "Informing other users about changed project"
      records <- getAllFromCache
      broadcast (U.toString projectUuid) records (toSetProjectMessage reqDto) disconnectUser
      logWS U.nil "Informed completed"

addEvent :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> ProjectEventChangeDTO -> m ()
addEvent projectUuid entityPerm mCreatedBy reqDto = do
  currentTenantUuid <- asks (.tenantUuid')
  serverConfig <- asks (.serverConfig')
  if isJust serverConfig.cloud.signalBridgeUrl
    then do
      now <- liftIO getCurrentTime
      let resDto = toEventDTO' reqDto mCreatedBy now
      let dto =
            AKM.fromList
              [ ("projectUuid", A.String . U.toText $ projectUuid)
              , ("tenantUuid", A.String . U.toText $ currentTenantUuid)
              , ("message", A.toJSON resDto)
              ]
      invokeLambda serverConfig.signalBridge.addEventArn (BSL.toStrict . A.encode $ dto)
      return ()
    else do
      logWS U.nil "Informing other users about new event"
      processSetContent projectUuid entityPerm mCreatedBy reqDto
      logWS U.nil "Informed completed"

addFile :: WizardRequestContextC s m => U.UUID -> ProjectFileSimple -> m ()
addFile projectUuid reqDto = do
  currentTenantUuid <- asks (.tenantUuid')
  serverConfig <- asks (.serverConfig')
  if isJust serverConfig.cloud.signalBridgeUrl
    then do
      let dto =
            AKM.fromList
              [ ("projectUuid", A.String . U.toText $ projectUuid)
              , ("tenantUuid", A.String . U.toText $ currentTenantUuid)
              , ("message", A.toJSON reqDto)
              ]
      invokeLambda serverConfig.signalBridge.addFileArn (BSL.toStrict . A.encode $ dto)
      return ()
    else do
      logWS U.nil "Informing other users about added file"
      records <- getAllFromCache
      broadcast (U.toString projectUuid) records (toAddFileMessage reqDto) disconnectUser
      logWS U.nil "Informed completed"

logOutOnlineUsersWhenProjectDramaticallyChanged :: WizardRequestContextC s m => U.UUID -> m ()
logOutOnlineUsersWhenProjectDramaticallyChanged projectUuid = do
  currentTenantUuid <- asks (.tenantUuid')
  serverConfig <- asks (.serverConfig')
  if isJust serverConfig.cloud.signalBridgeUrl
    then do
      let dto = AKM.fromList [("projectUuid", U.toString projectUuid), ("tenantUuid", U.toString currentTenantUuid)]
      invokeLambda serverConfig.signalBridge.logOutAllArn (BSL.toStrict . A.encode $ dto)
      return ()
    else do
      records <- getAllFromCache
      let error = NotExistsError $ _ERROR_SERVICE_PROJECT_COLLABORATION__FORCE_DISCONNECT (U.toString projectUuid)
      traverse_ (logOut error) records
  where
    logOut error record =
      when
        (record.entityId == U.toString projectUuid)
        (sendError record.connectionUuid record.connection record.entityId disconnectUser error)

-- --------------------------------
setContent :: WizardRequestContextC s m => U.UUID -> U.UUID -> ProjectEventChangeDTO -> m ()
setContent projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  let mCreatedBy = getMaybeCreatedBy myself
  processSetContent projectUuid myself.entityPerm mCreatedBy reqDto

processSetContent :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> ProjectEventChangeDTO -> m ()
processSetContent projectUuid perm mCreatedBy reqDto =
  case reqDto of
    SetReplyEventChangeDTO' event -> setReply projectUuid perm mCreatedBy event
    ClearReplyEventChangeDTO' event -> clearReply projectUuid perm mCreatedBy event
    SetPhaseEventChangeDTO' event -> setPhase projectUuid perm mCreatedBy event
    SetLabelsEventChangeDTO' event -> setLabel projectUuid perm mCreatedBy event
    ResolveCommentThreadEventChangeDTO' event -> resolveCommentThread projectUuid perm mCreatedBy event
    ReopenCommentThreadEventChangeDTO' event -> reopenCommentThread projectUuid perm mCreatedBy event
    AssignCommentThreadEventChangeDTO' event -> assignCommentThread projectUuid perm mCreatedBy event
    DeleteCommentThreadEventChangeDTO' event -> deleteCommentThread projectUuid perm mCreatedBy event
    AddCommentEventChangeDTO' event -> addComment projectUuid perm mCreatedBy event
    EditCommentEventChangeDTO' event -> editComment projectUuid perm mCreatedBy event
    DeleteCommentEventChangeDTO' event -> deleteComment projectUuid perm mCreatedBy event

setReply :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> SetReplyEventChangeDTO -> m ()
setReply projectUuid entityPerm mCreatedBy reqDto = do
  checkEditPermission entityPerm
  now <- liftIO getCurrentTime
  tenantUuid <- asks (.tenantUuid')
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  insertProjectEventWithTimestampUpdate
    projectUuid
    (fromEventChangeDTO (SetReplyEventChangeDTO' reqDto) projectUuid tenantUuid mCreatedByUuid now)
  let resDto = toSetReplyEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString projectUuid) records (toSetReplyMessage resDto) disconnectUser

clearReply :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> ClearReplyEventChangeDTO -> m ()
clearReply projectUuid entityPerm mCreatedBy reqDto = do
  checkEditPermission entityPerm
  now <- liftIO getCurrentTime
  tenantUuid <- asks (.tenantUuid')
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  insertProjectEventWithTimestampUpdate
    projectUuid
    (fromEventChangeDTO (ClearReplyEventChangeDTO' reqDto) projectUuid tenantUuid mCreatedByUuid now)
  let resDto = toClearReplyEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString projectUuid) records (toClearReplyMessage resDto) disconnectUser

setPhase :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> SetPhaseEventChangeDTO -> m ()
setPhase projectUuid entityPerm mCreatedBy reqDto = do
  checkEditPermission entityPerm
  now <- liftIO getCurrentTime
  tenantUuid <- asks (.tenantUuid')
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  insertProjectEventWithTimestampUpdate
    projectUuid
    (fromEventChangeDTO (SetPhaseEventChangeDTO' reqDto) projectUuid tenantUuid mCreatedByUuid now)
  let resDto = toSetPhaseEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString projectUuid) records (toSetPhaseMessage resDto) disconnectUser

setLabel :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> SetLabelsEventChangeDTO -> m ()
setLabel projectUuid entityPerm mCreatedBy reqDto = do
  checkEditPermission entityPerm
  now <- liftIO getCurrentTime
  tenantUuid <- asks (.tenantUuid')
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  insertProjectEventWithTimestampUpdate projectUuid (fromEventChangeDTO (SetLabelsEventChangeDTO' reqDto) projectUuid tenantUuid mCreatedByUuid now)
  let resDto = toSetLabelsEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString projectUuid) records (toSetLabelMessage resDto) disconnectUser

resolveCommentThread :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> ResolveCommentThreadEventChangeDTO -> m ()
resolveCommentThread projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  updateProjectCommentThreadResolvedById reqDto.threadUuid True
  let resDto = toResolveCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toResolveCommentThreadMessage resDto) disconnectUser

reopenCommentThread :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> ReopenCommentThreadEventChangeDTO -> m ()
reopenCommentThread projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  updateProjectCommentThreadResolvedById reqDto.threadUuid False
  let resDto = toReopenCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toReopenCommentThreadMessage resDto) disconnectUser

assignCommentThread :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> AssignCommentThreadEventChangeDTO -> m ()
assignCommentThread projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  updateProjectCommentThreadAssignee reqDto.threadUuid (fmap (.uuid) reqDto.assignedTo) mCreatedByUuid
  let resDto = toAssignCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toAssignCommentThreadMessage resDto) disconnectUser

deleteCommentThread :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> DeleteCommentThreadEventChangeDTO -> m ()
deleteCommentThread projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  deleteProjectCommentsByThreadUuid reqDto.threadUuid
  deleteProjectCommentThreadById reqDto.threadUuid
  deleteProjectCacheByProjectUuid projectUuid
  let resDto = toDeleteCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toDeleteCommentThreadMessage resDto) disconnectUser

addComment :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> AddCommentEventChangeDTO -> m ()
addComment projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  let comment = toComment reqDto tenantUuid mCreatedByUuid now
  if reqDto.newThread
    then do
      let thread = toCommentThread reqDto projectUuid tenantUuid mCreatedByUuid now
      insertProjectThreadAndComment thread comment
    else insertProjectComment comment
  let resDto = toAddCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toAddCommentMessage resDto) disconnectUser

editComment :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> EditCommentEventChangeDTO -> m ()
editComment projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  updateProjectCommentTextById reqDto.commentUuid reqDto.text
  let resDto = toEditCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toEditCommentMessage resDto) disconnectUser

deleteComment :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> DeleteCommentEventChangeDTO -> m ()
deleteComment projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  deleteProjectCommentById reqDto.commentUuid
  deleteProjectCacheByProjectUuid projectUuid
  let resDto = toDeleteCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toDeleteCommentMessage resDto) disconnectUser

-- --------------------------------
-- PRIVATE
-- --------------------------------
disconnectUser :: (WizardRequestContextC s m, A.ToJSON resDto) => WebsocketMessage resDto -> m ()
disconnectUser msg = deleteUser (u' msg.entityId) msg.connectionUuid

disconnectUserIfLostPermission :: WizardRequestContextC s m => WebsocketRecord -> m ()
disconnectUserIfLostPermission record = catchError (checkViewPermission record.entityPerm) handleError
  where
    handleError = sendError record.connectionUuid record.connection record.entityId disconnectUser

createProjectRecord :: WizardRequestContextC s m => U.UUID -> Connection -> U.UUID -> m WebsocketRecord
createProjectRecord connectionUuid connection projectUuid = do
  mCurrentUser <- asks (.currentUser')
  project <- findProjectByUuid projectUuid
  userGroupUuids <-
    case mCurrentUser of
      Just currentUser -> do
        userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
        return . fmap (.userGroupUuid) $ userGroupMemberships
      Nothing -> return []
  let permission =
        getPermission
          project.visibility
          project.sharing
          project.permissions
          (fmap (.uuid) mCurrentUser)
          (maybe [] (.role.permissions) mCurrentUser)
          userGroupUuids
  createRecord connectionUuid connection (U.toString projectUuid) permission userGroupUuids

getMaybeCreatedBy :: WebsocketRecord -> Maybe UserSuggestion
getMaybeCreatedBy myself =
  case myself.user of
    u@LoggedOnlineUserInfo
      { uuid = uuid
      , firstName = firstName
      , lastName = lastName
      , gravatarHash = gravatarHash
      , imageUrl = imageUrl
      , affiliation = affiliation
      } ->
        Just $
          UserSuggestion
            { uuid = uuid
            , firstName = firstName
            , lastName = lastName
            , gravatarHash = gravatarHash
            , imageUrl = imageUrl
            , affiliation = affiliation
            }
    u@AnonymousOnlineUserInfo {..} -> Nothing
