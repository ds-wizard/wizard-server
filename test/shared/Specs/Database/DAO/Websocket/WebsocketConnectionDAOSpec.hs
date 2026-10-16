module Specs.Database.DAO.Websocket.WebsocketConnectionDAOSpec where

import Database.PostgreSQL.Simple (execute_)
import Test.Hspec

import Shared.Constant.Tenant
import Shared.Database.DAO.Common
import Shared.Database.DAO.Websocket.WebsocketConnectionDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.User.OnlineUserInfo
import Shared.Model.User.User
import Shared.Model.Websocket.WebsocketConnection
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketSubscription
import Shared.Util.Uuid

import Specs.Common

node1 = u' "1a000000-0000-0000-0000-000000000001"

node2 = u' "1a000000-0000-0000-0000-000000000002"

project1 = u' "2b000000-0000-0000-0000-000000000001"

project2 = u' "2b000000-0000-0000-0000-000000000002"

connection1 =
  WebsocketConnection
    { uuid = u' "3c000000-0000-0000-0000-000000000001"
    , nodeUuid = node1
    , tenantUuid = defaultTenantUuid
    , subscription = ProjectWebsocketSubscription
    , entityUuid = project1
    , userUuid = Just userAlbert.uuid
    , websocketPerm = EditorWebsocketPerm
    , userGroupUuids = [u' "4d000000-0000-0000-0000-000000000001"]
    , onlineUser = AnonymousOnlineUserInfo {avatarNumber = 3, colorNumber = 5}
    }

connection2 =
  connection1
    { uuid = u' "3c000000-0000-0000-0000-000000000002"
    , nodeUuid = node2
    , entityUuid = project2
    , userUuid = Nothing
    , websocketPerm = ViewerWebsocketPerm
    , userGroupUuids = []
    }

connection3 =
  connection1
    { uuid = u' "3c000000-0000-0000-0000-000000000003"
    , nodeUuid = node2
    , subscription = AnalyticsWebsocketSubscription
    , entityUuid = defaultTenantUuid
    }

websocketConnectionDAOSpec requestContext =
  describe "WebsocketConnection DAO" $ do
    it "finds the connections of one entity" $ do
      (Right _) <- runInContext (traverse insertWebsocketConnection [connection1, connection2, connection3]) requestContext
      (Right found) <- runInContext (findWebsocketConnectionsByEntity defaultTenantUuid ProjectWebsocketSubscription project1) requestContext
      found `shouldBe` [connection1]
      (Right other) <- runInContext (findWebsocketConnectionsByEntity differentTenantUuid ProjectWebsocketSubscription project1) requestContext
      other `shouldBe` []
    it "updates the permission" $ do
      (Right _) <- runInContext (insertWebsocketConnection connection1) requestContext
      (Right _) <- runInContext (updateWebsocketConnectionPermByUuid connection1.uuid CommenterWebsocketPerm) requestContext
      (Right [found]) <- runInContext (findWebsocketConnectionsByEntity defaultTenantUuid ProjectWebsocketSubscription project1) requestContext
      found.websocketPerm `shouldBe` CommenterWebsocketPerm
    it "removes a user group from the connections of the users on one node" $ do
      let groupUuid = u' "4d000000-0000-0000-0000-000000000001"
      let user = LoggedOnlineUserInfo {uuid = userAlbert.uuid, firstName = "Albert", lastName = "Einstein", gravatarHash = "", imageUrl = Nothing, affiliation = Nothing, colorNumber = 1, role = RoleSimple {uuid = userAlbert.uuid, name = "Admin", permissions = []}, groupUuids = [groupUuid]}
      (Right _) <- runInContext (insertWebsocketConnection connection1 {onlineUser = user}) requestContext
      (Right rows) <- runInContext (removeUserGroupFromWebsocketConnections node1 defaultTenantUuid groupUuid [userAlbert.uuid]) requestContext
      rows `shouldBe` [(connection1.uuid, project1)]
      (Right [found]) <- runInContext (findWebsocketConnectionsByEntity defaultTenantUuid ProjectWebsocketSubscription project1) requestContext
      found.userGroupUuids `shouldBe` []
      found.onlineUser `shouldBe` user {groupUuids = []}
    it "refreshes the heartbeat of the given connections and purges the stale rows" $ do
      (Right _) <- runInContext (traverse insertWebsocketConnection [connection1, connection2, connection3]) requestContext
      (Right _) <- runInContext (runDB (`execute_` "UPDATE websocket_connection SET heartbeat_at = now() - interval '3 minutes'")) requestContext
      (Right refreshed) <- runInContext (updateWebsocketConnectionHeartbeat node2 [connection1.uuid, connection2.uuid]) requestContext
      refreshed `shouldBe` [connection2.uuid]
      (Right purged) <- runInContext deleteStaleWebsocketConnections requestContext
      purged `shouldMatchList` [(defaultTenantUuid, ProjectWebsocketSubscription, project1), (defaultTenantUuid, AnalyticsWebsocketSubscription, defaultTenantUuid)]
      (Right count) <- runInContext countWebsocketConnections requestContext
      count `shouldBe` 1
    it "deletes by uuid and by node" $ do
      (Right _) <- runInContext (traverse insertWebsocketConnection [connection1, connection2, connection3]) requestContext
      (Right _) <- runInContext (deleteWebsocketConnectionByUuid connection1.uuid) requestContext
      (Right _) <- runInContext (deleteWebsocketConnectionsByNodeUuid node2) requestContext
      (Right count) <- runInContext countWebsocketConnections requestContext
      count `shouldBe` 0
