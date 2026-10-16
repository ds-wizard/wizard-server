module Specs.Api.Handler.Project.Detail_WS.PresenceSpec where

import Data.Aeson
import qualified Data.UUID as U
import Network.WebSockets (Connection)
import Test.Hspec hiding (expectationFailure, shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Constant.Workspace
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Public
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.User.OnlineUserInfo
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.Workspace.WorkspaceMembershipService
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Detail_WS.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

presenceSpec requestContext =
  describe "presence" $ do
    test_userList requestContext
    test_permissionChange requestContext
    test_userGroupRemoval requestContext
    test_workspaceMembershipRemoval requestContext
    test_invalidToken requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_userList requestContext =
  it "WS 200 OK - user list follows connects and disconnects" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      -- WHEN: Connect 1. user
      (c1, s1) <- createConnection requestContext (reqUrlT project.uuid (Just reqAuthToken))
      -- THEN:
      read_SetUserList c1 0
      -- WHEN: Connect 2. user
      (c2, s2) <- createConnection requestContext (reqUrlT project.uuid (Just reqNonAdminAuthToken))
      -- THEN:
      read_SetUserList c2 1
      read_SetUserList c1 1
      assertCountOfWebsocketConnection requestContext 2
      -- WHEN: Disconnect 2. user
      closeConnection c2
      -- THEN:
      read_SetUserList c1 0
      assertCountOfWebsocketConnectionEventually requestContext 1
      -- AND: Close sockets
      closeSockets [s1, s2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_permissionChange requestContext =
  it "WS 403 FORBIDDEN - viewer is disconnected after the project visibility changes" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project.uuid
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Project"
      -- WHEN: Update permission
      runInContextIO (updateProjectByUuid project {visibility = PrivateProjectVisibility, sharing = RestrictedProjectSharing}) requestContext
      runInContext (updatePermsForOnlineUsers project.uuid) requestContext
      -- THEN: Read response
      read_Error c2 expError
      read_Closed c2 1008
      read_Error c3 expError
      read_Closed c3 1008
      read_SetUserList c1 0
      assertCountOfWebsocketConnection requestContext 1
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_userGroupRemoval requestContext =
  it "WS 200 OK - the user list no longer shows a removed user group" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project.uuid
      -- WHEN: Remove the user group from Nikola
      runInContext (removeUserGroupFromUsers bioGroup.uuid [userNikola.uuid]) requestContext
      -- AND: The anonymous user leaves
      closeConnection c3
      -- THEN:
      users <- read_SetUserListUsers c1
      fmap groupUuidsOf users `shouldBe` [Just []]
      read_SetUserList c2 1
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_workspaceMembershipRemoval requestContext =
  it "WS 403 FORBIDDEN - viewer whose access came from the workspace is disconnected after leaving it" $
    -- GIVEN: Prepare database
    do
      let project = project10 {sharing = RestrictedProjectSharing, permissions = []}
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      -- AND: Connect to websocket
      (c1, s1) <- createConnection requestContext (reqUrlT project.uuid (Just reqAuthToken))
      read_SetUserList c1 0
      (c2, s2) <- createConnection requestContext (reqUrlT project.uuid (Just reqIsaacAuthToken))
      read_SetUserList c2 1
      read_SetUserList c1 1
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Project"
      -- WHEN: Remove Isaac from the workspace
      runInContext (removeWorkspaceMembership defaultWorkspaceUuid userIsaac.uuid) requestContext
      -- THEN:
      read_Error c2 expError
      read_Closed c2 1008
      read_SetUserList c1 0
      assertCountOfWebsocketConnectionEventually requestContext 1
      -- AND: Close sockets
      closeSockets [s1, s2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_invalidToken requestContext =
  it "WS 401 UNAUTHORIZED - invalid token rejects the handshake" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      -- WHEN:
      -- THEN:
      read_Rejected requestContext.serverConfig.general.serverPort (reqUrlT project.uuid (Just "invalid-token")) 401
      assertCountOfWebsocketConnection requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
read_SetUserListUsers :: Connection -> IO [OnlineUserInfo]
read_SetUserListUsers connection = do
  resDto <- receiveDataWithin connection
  case eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO) of
    Right (Success_ServerActionDTO (SetUserList_ServerProjectMessageDTO users)) -> return users
    other -> expectationFailure ("Expected a user list, got: " ++ show other) >> return []

groupUuidsOf :: OnlineUserInfo -> Maybe [U.UUID]
groupUuidsOf LoggedOnlineUserInfo {groupUuids = groupUuids} = Just groupUuids
groupUuidsOf AnonymousOnlineUserInfo {} = Nothing
