module Shared.Service.Project.Collaboration.ProjectCollaborationService where

import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO, local)
import Data.Aeson (ToJSON, toJSON)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.Project.ProjectCommentDAO
import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Model.Project.Project
import Shared.Model.User.OnlineUserInfo
import Shared.Model.User.UserGroupMembership
import Shared.Model.User.UserSuggestion
import Shared.Model.Websocket.WebsocketBusMessage
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Model.Websocket.WebsocketSubscription
import Shared.Service.Acl.AclService
import Shared.Service.Project.Collaboration.ProjectCollaborationAcl
import Shared.Service.Project.Collaboration.ProjectCollaborationMapper
import Shared.Service.Project.Comment.ProjectCommentMapper
import Shared.Service.Project.Event.ProjectEventMapper
import Shared.Service.Websocket.WebsocketBusService
import Shared.Service.Websocket.WebsocketRegistryService
import Shared.Service.Websocket.WebsocketService
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.Websocket

putUserOnline :: WizardRequestContextC s m => U.UUID -> U.UUID -> WebsocketOutbound -> m ()
putUserOnline projectUuid connectionUuid outbound = do
  (mWorkspaceUser, permission, userGroupUuids) <- resolveProjectPermission projectUuid
  checkViewPermission permission
  key <- projectKey projectUuid
  mCurrentUser <- asks (.currentUser')
  let tenantRolePermissions = maybe [] (.role.permissions) mCurrentUser
  local (setCurrentUser mWorkspaceUser) (connectUser key connectionUuid outbound permission userGroupUuids tenantRolePermissions)
  logWS connectionUuid "New user added to the list"
  publishUserListChanged key

deleteUser :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
deleteUser projectUuid connectionUuid = do
  key <- projectKey projectUuid
  present <- disconnectUser key connectionUuid
  when present (publishUserListChanged key)

updatePermsForOnlineUsers :: WizardRequestContextC s m => U.UUID -> m ()
updatePermsForOnlineUsers projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  publish ProjectPermissionsChanged {..}

updatePermsForWorkspace :: WizardRequestContextC s m => U.UUID -> m ()
updatePermsForWorkspace workspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  publish WorkspacePermissionsChanged {..}

removeUserGroupFromUsers :: WizardRequestContextC s m => U.UUID -> [U.UUID] -> m ()
removeUserGroupFromUsers userGroupUuid userUuids = do
  tenantUuid <- asks (.tenantUuid')
  publish UserGroupRemoved {..}

setProject :: WizardRequestContextC s m => U.UUID -> ProjectDetailWsDTO -> m ()
setProject projectUuid reqDto = broadcast projectUuid AllRecipients (toSetProjectMessage reqDto)

addEvent :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> ProjectEventChangeDTO -> m ()
addEvent = processSetContent

addFile :: WizardRequestContextC s m => U.UUID -> ProjectFileSimple -> m ()
addFile projectUuid reqDto = broadcast projectUuid AllRecipients (toAddFileMessage reqDto)

logOutOnlineUsersWhenProjectDramaticallyChanged :: WizardRequestContextC s m => U.UUID -> m ()
logOutOnlineUsersWhenProjectDramaticallyChanged projectUuid = do
  (tenantUuid, subscription, entityUuid) <- projectKey projectUuid
  publish LogOutAll {..}

-- --------------------------------
setContent :: WizardRequestContextC s m => U.UUID -> U.UUID -> ProjectEventChangeDTO -> m ()
setContent projectUuid connectionUuid reqDto = do
  key <- projectKey projectUuid
  myself <- findLocalConnection' key connectionUuid
  processSetContent projectUuid myself.websocketPerm (getMaybeCreatedBy myself.onlineUser) reqDto

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
  now <- insertEvent projectUuid mCreatedBy (SetReplyEventChangeDTO' reqDto)
  broadcast projectUuid AllRecipients (toSetReplyMessage (toSetReplyEventDTO' reqDto mCreatedBy now))

clearReply :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> ClearReplyEventChangeDTO -> m ()
clearReply projectUuid entityPerm mCreatedBy reqDto = do
  checkEditPermission entityPerm
  now <- insertEvent projectUuid mCreatedBy (ClearReplyEventChangeDTO' reqDto)
  broadcast projectUuid AllRecipients (toClearReplyMessage (toClearReplyEventDTO' reqDto mCreatedBy now))

setPhase :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> SetPhaseEventChangeDTO -> m ()
setPhase projectUuid entityPerm mCreatedBy reqDto = do
  checkEditPermission entityPerm
  now <- insertEvent projectUuid mCreatedBy (SetPhaseEventChangeDTO' reqDto)
  broadcast projectUuid AllRecipients (toSetPhaseMessage (toSetPhaseEventDTO' reqDto mCreatedBy now))

setLabel :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> SetLabelsEventChangeDTO -> m ()
setLabel projectUuid entityPerm mCreatedBy reqDto = do
  checkEditPermission entityPerm
  now <- insertEvent projectUuid mCreatedBy (SetLabelsEventChangeDTO' reqDto)
  broadcast projectUuid AllRecipients (toSetLabelMessage (toSetLabelsEventDTO' reqDto mCreatedBy now))

resolveCommentThread :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> ResolveCommentThreadEventChangeDTO -> m ()
resolveCommentThread projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  updateProjectCommentThreadResolvedById reqDto.threadUuid True
  broadcast projectUuid (commentRecipients reqDto.private) (toResolveCommentThreadMessage (toResolveCommentThreadEventDTO' reqDto mCreatedBy now))

reopenCommentThread :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> ReopenCommentThreadEventChangeDTO -> m ()
reopenCommentThread projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  updateProjectCommentThreadResolvedById reqDto.threadUuid False
  broadcast projectUuid (commentRecipients reqDto.private) (toReopenCommentThreadMessage (toReopenCommentThreadEventDTO' reqDto mCreatedBy now))

assignCommentThread :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> AssignCommentThreadEventChangeDTO -> m ()
assignCommentThread projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  updateProjectCommentThreadAssignee reqDto.threadUuid (fmap (.uuid) reqDto.assignedTo) (fmap (.uuid) mCreatedBy)
  broadcast projectUuid (commentRecipients reqDto.private) (toAssignCommentThreadMessage (toAssignCommentThreadEventDTO' reqDto mCreatedBy now))

deleteCommentThread :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> DeleteCommentThreadEventChangeDTO -> m ()
deleteCommentThread projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  deleteProjectCommentsByThreadUuid reqDto.threadUuid
  deleteProjectCommentThreadById reqDto.threadUuid
  deleteProjectCacheByProjectUuid projectUuid
  broadcast projectUuid (commentRecipients reqDto.private) (toDeleteCommentThreadMessage (toDeleteCommentThreadEventDTO' reqDto mCreatedBy now))

addComment :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> AddCommentEventChangeDTO -> m ()
addComment projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let mCreatedByUuid = fmap (.uuid) mCreatedBy
  let comment = toComment reqDto tenantUuid mCreatedByUuid now
  if reqDto.newThread
    then insertProjectThreadAndComment (toCommentThread reqDto projectUuid tenantUuid mCreatedByUuid now) comment
    else insertProjectComment comment
  broadcast projectUuid (commentRecipients reqDto.private) (toAddCommentMessage (toAddCommentEventDTO' reqDto mCreatedBy now))

editComment :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> EditCommentEventChangeDTO -> m ()
editComment projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  updateProjectCommentTextById reqDto.commentUuid reqDto.text
  broadcast projectUuid (commentRecipients reqDto.private) (toEditCommentMessage (toEditCommentEventDTO' reqDto mCreatedBy now))

deleteComment :: WizardRequestContextC s m => U.UUID -> WebsocketPerm -> Maybe UserSuggestion -> DeleteCommentEventChangeDTO -> m ()
deleteComment projectUuid entityPerm mCreatedBy reqDto = do
  checkCommentPermission entityPerm
  now <- liftIO getCurrentTime
  deleteProjectCommentById reqDto.commentUuid
  deleteProjectCacheByProjectUuid projectUuid
  broadcast projectUuid (commentRecipients reqDto.private) (toDeleteCommentMessage (toDeleteCommentEventDTO' reqDto mCreatedBy now))

-- --------------------------------
-- PRIVATE
-- --------------------------------
projectKey :: WizardRequestContextC s m => U.UUID -> m WebsocketEntityKey
projectKey projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  return (tenantUuid, ProjectWebsocketSubscription, projectUuid)

broadcast :: (WizardRequestContextC s m, ToJSON content) => U.UUID -> WebsocketRecipients -> content -> m ()
broadcast projectUuid recipients content = do
  (tenantUuid, subscription, entityUuid) <- projectKey projectUuid
  publish Broadcast {payload = toJSON content, ..}

commentRecipients :: Bool -> WebsocketRecipients
commentRecipients private = if private then EditorsOnly else CommentersAndEditors

insertEvent :: WizardRequestContextC s m => U.UUID -> Maybe UserSuggestion -> ProjectEventChangeDTO -> m UTCTime
insertEvent projectUuid mCreatedBy reqDto = do
  now <- liftIO getCurrentTime
  tenantUuid <- asks (.tenantUuid')
  insertProjectEventWithTimestampUpdate projectUuid (fromEventChangeDTO reqDto projectUuid tenantUuid (fmap (.uuid) mCreatedBy) now)
  return now

resolveProjectPermission :: WizardRequestContextC s m => U.UUID -> m (Maybe UserDTO, WebsocketPerm, [U.UUID])
resolveProjectPermission projectUuid = do
  mCurrentUser <- asks (.currentUser')
  workspaceRoles <- asks (.workspaceRoles')
  project <- findProjectByUuid projectUuid
  userGroupUuids <- maybe (return []) (fmap (fmap (.userGroupUuid)) . findUserGroupMembershipsByUserUuid . (.uuid)) mCurrentUser
  multiWorkspace <- asks (.tenantMultiWorkspace')
  reachable <- isWorkspaceReachable project.workspaceUuid
  let withWorkspacePermissions currentUser = currentUser {role = currentUser.role {permissions = permissionsInWorkspace currentUser workspaceRoles multiWorkspace (Just project.workspaceUuid)}} :: UserDTO
  let mWorkspaceUser = fmap withWorkspacePermissions mCurrentUser
  let permission =
        getPermission
          project.visibility
          project.sharing
          project.permissions
          (fmap (.uuid) mWorkspaceUser)
          (maybe [] (.role.permissions) mWorkspaceUser)
          userGroupUuids
          reachable
  return (mWorkspaceUser, permission, userGroupUuids)

getMaybeCreatedBy :: OnlineUserInfo -> Maybe UserSuggestion
getMaybeCreatedBy LoggedOnlineUserInfo {..} = Just UserSuggestion {..}
getMaybeCreatedBy AnonymousOnlineUserInfo {} = Nothing
