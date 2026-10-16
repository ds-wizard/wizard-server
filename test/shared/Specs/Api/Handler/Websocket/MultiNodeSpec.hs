module Specs.Api.Handler.Websocket.MultiNodeSpec where

import Control.Monad (void)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Only (..), execute_, query_)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status (ok200)
import Network.Socket (Socket)
import Network.WebSockets (Connection)
import Test.Hspec hiding (shouldBe, shouldReturn)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Shared.Api.Resource.Project.ProjectSettingsChangeDTO
import Shared.Constant.Tenant
import Shared.Database.DAO.Common
import Shared.Database.DAO.Websocket.WebsocketConnectionDAO
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Http.HttpRequest
import Shared.Model.Project.Project
import Shared.Model.User.OnlineUserInfo
import Shared.Model.Websocket.WebsocketConnection
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketSubscription
import Shared.Service.Websocket.WebsocketBusService
import Shared.Service.Websocket.WebsocketNodeService
import Shared.Util.String (f')
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Detail_WS.Common
import Specs.Api.Handler.Project.Detail_WS.DeliverySpec (read_SetReplyValue, setReplyEvent, write_SetReply)
import Specs.Api.Handler.Project.Detail_WS.SetProjectSpec (read_SetProject)
import Specs.Api.Handler.Websocket.Common
import Specs.Common

multiNodeSpec :: RequestContext -> SpecWith ()
multiNodeSpec requestContext =
  describe "multi-node" $ do
    test_userListSpansNodes requestContext
    test_eventCrossesNodes requestContext
    test_outbox requestContext
    test_stalePurge requestContext
    test_orphanRowPurge requestContext
    test_purgedConnectionClosed requestContext
    test_listenerRestart requestContext
    test_shutdown requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_userListSpansNodes :: RequestContext -> SpecWith ()
test_userListSpansNodes requestContext =
  it "WS 200 OK - the user list spans both nodes" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      withSecondNode requestContext $ \requestContextB -> do
        -- AND: Connect one client to node A
        (cA, sA) <- createConnection requestContext (reqUrlT project.uuid (Just reqAuthToken))
        read_SetUserList cA 0
        -- WHEN: Connect one client to node B
        (cB, sB) <- createConnection requestContextB (reqUrlT project.uuid (Just reqNonAdminAuthToken))
        -- THEN:
        read_SetUserList cB 1
        read_SetUserList cA 1
        assertCountOfWebsocketConnection requestContext 2
        -- AND: Close sockets
        closeSockets [sA, sB]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_eventCrossesNodes :: RequestContext -> SpecWith ()
test_eventCrossesNodes requestContext =
  it "WS 200 OK - an event sent on node A arrives on node B" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      withSecondNode requestContext $ \requestContextB -> do
        -- AND: Connect one client to each node
        ((cA, sA), (cB, sB)) <- connectToBothNodes requestContext requestContextB project.uuid
        event <- setReplyEvent project.uuid "crossed"
        -- WHEN:
        write_SetReply cA event
        -- THEN:
        read_SetReplyValue cB event
        read_SetReplyValue cA event
        -- AND: Close sockets
        closeSockets [sA, sB]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_outbox :: RequestContext -> SpecWith ()
test_outbox requestContext =
  it "WS 200 OK - a project update over 7 KB crosses through the outbox" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      runInContext (createDeleteEntitiesFn "websocket_outbox") requestContext
      clearConnections requestContext project.uuid
      withSecondNode requestContext $ \requestContextB -> do
        -- AND: Connect one client to each node
        ((cA, sA), (cB, sB)) <- connectToBothNodes requestContext requestContextB project.uuid
        -- AND: Prepare request
        let description = Just (replicate 8000 'x')
        let reqDto = project10EditedSettingsChange {description = description} :: ProjectSettingsChangeDTO
        let request =
              HttpRequest
                { requestMethod = "PUT"
                , requestUrl = "/api/projects/" ++ U.toString project.uuid ++ "/settings"
                , requestHeaders = M.fromList [("Authorization", "Bearer " ++ reqAuthToken), ("Content-Type", "application/json")]
                , requestBody = BSL.toStrict . encode $ reqDto
                , multipart = Nothing
                }
        -- WHEN:
        (Right response) <- runSimpleRequest requestContext request
        -- THEN:
        HC.responseStatus response `shouldBe` ok200
        let expDto = project10EditedWs {description = description} :: ProjectDetailWsDTO
        read_SetProject cB expDto
        read_SetProject cA expDto
        -- AND: The payload went through the outbox
        countOutbox requestContext `shouldReturn` 1
        -- AND: The cleanup removes old outbox rows
        execute_' requestContext "UPDATE websocket_outbox SET created_at = now() - interval '6 minutes'"
        runInContext purgeWebsocketOutbox requestContext
        countOutbox requestContext `shouldReturn` 0
        -- AND: Close sockets
        closeSockets [sA, sB]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_stalePurge :: RequestContext -> SpecWith ()
test_stalePurge requestContext =
  it "WS 200 OK - the users of a stale node disappear from the list" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      withSecondNode requestContext $ \requestContextB -> do
        -- AND: Connect one client to each node
        ((cA, sA), (_, sB)) <- connectToBothNodes requestContext requestContextB project.uuid
        -- AND: Node B stops sending heartbeats
        execute_' requestContext (f' "UPDATE websocket_connection SET heartbeat_at = now() - interval '3 minutes' WHERE node_uuid = '%s'" [U.toString requestContextB.nodeUuid])
        -- WHEN:
        runInContext purgeStaleWebsocketConnections requestContext
        -- THEN:
        read_SetUserList cA 0
        assertCountOfWebsocketConnection requestContext 1
        -- AND: Close sockets
        closeSockets [sA, sB]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_orphanRowPurge :: RequestContext -> SpecWith ()
test_orphanRowPurge requestContext =
  it "WS 200 OK - a presence row without a local connection is not kept alive by the heartbeat" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      (cA, sA) <- createConnection requestContext (reqUrlT project.uuid (Just reqAuthToken))
      read_SetUserList cA 0
      -- AND: A row of node A whose connection is gone
      orphanUuid <- generateUuid
      runInContext (insertWebsocketConnection (orphanConnection requestContext project.uuid orphanUuid)) requestContext
      -- AND: Every row of node A is old
      execute_' requestContext "UPDATE websocket_connection SET heartbeat_at = now() - interval '3 minutes'"
      -- WHEN:
      runInContext refreshNodeHeartbeat requestContext
      runInContext purgeStaleWebsocketConnections requestContext
      -- THEN:
      read_SetUserList cA 0
      assertCountOfWebsocketConnection requestContext 1
      -- AND: Close sockets
      closeSockets [sA]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_purgedConnectionClosed :: RequestContext -> SpecWith ()
test_purgedConnectionClosed requestContext =
  it "WS 200 OK - a connection whose row another node purged is closed with 1012" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      withSecondNode requestContext $ \requestContextB -> do
        -- AND: Connect one client to each node
        ((cA, sA), (cB, sB)) <- connectToBothNodes requestContext requestContextB project.uuid
        -- AND: Another node purged the rows of node B
        execute_' requestContext (f' "DELETE FROM websocket_connection WHERE node_uuid = '%s'" [U.toString requestContextB.nodeUuid])
        -- WHEN:
        runInContext refreshNodeHeartbeat requestContextB
        -- THEN:
        read_CloseCode cB 1012
        -- AND: Close sockets
        closeSockets [sA, sB]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_listenerRestart :: RequestContext -> SpecWith ()
test_listenerRestart requestContext =
  it "WS 200 OK - a restarted bus listener closes the local connections with 1012" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      withSecondNode requestContext $ \requestContextB -> do
        -- AND: Connect one client to each node
        ((cA, sA), (cB, sB)) <- connectToBothNodes requestContext requestContextB project.uuid
        -- WHEN: The listening connection of node B is lost
        terminateNewestWebsocketListener requestContext
        -- THEN:
        waitForNode requestContextB 2
        read_CloseCode cB 1012
        read_SetUserList cA 0
        -- AND: Close sockets
        closeSockets [sA, sB]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_shutdown :: RequestContext -> SpecWith ()
test_shutdown requestContext =
  it "WS 200 OK - a node shutting down closes its connections with 1001 and removes its rows" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      clearConnections requestContext project.uuid
      -- AND: Connect one client to each node
      ((cA, sA), (cB, sB)) <- withSecondNode requestContext $ \requestContextB ->
        connectToBothNodes requestContext requestContextB project.uuid
      -- WHEN: Node B has shut down
      -- THEN:
      read_Closed cB 1001
      read_SetUserList cA 0
      assertCountOfWebsocketConnection requestContext 1
      -- AND: Close sockets
      closeSockets [sA, sB]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
connectToBothNodes :: RequestContext -> RequestContext -> U.UUID -> IO ((Connection, Socket), (Connection, Socket))
connectToBothNodes requestContext requestContextB projectUuid = do
  (cA, sA) <- createConnection requestContext (reqUrlT projectUuid (Just reqAuthToken))
  read_SetUserList cA 0
  (cB, sB) <- createConnection requestContextB (reqUrlT projectUuid (Just reqNonAdminAuthToken))
  read_SetUserList cB 1
  read_SetUserList cA 1
  return ((cA, sA), (cB, sB))

countOutbox :: RequestContext -> IO Int
countOutbox requestContext = do
  (Right count) <- runInContext (createCountFn "websocket_outbox") requestContext
  return (fromIntegral count)

execute_' :: RequestContext -> String -> IO ()
execute_' requestContext sql = void $ runInContext (runDB (\conn -> execute_ conn (fromString sql))) requestContext

orphanConnection :: RequestContext -> U.UUID -> U.UUID -> WebsocketConnection
orphanConnection requestContext projectUuid uuid =
  WebsocketConnection
    { uuid = uuid
    , nodeUuid = requestContext.nodeUuid
    , tenantUuid = defaultTenantUuid
    , subscription = ProjectWebsocketSubscription
    , entityUuid = projectUuid
    , userUuid = Nothing
    , websocketPerm = ViewerWebsocketPerm
    , userGroupUuids = []
    , onlineUser = AnonymousOnlineUserInfo {avatarNumber = 1, colorNumber = 1}
    }

terminateNewestWebsocketListener :: RequestContext -> IO ()
terminateNewestWebsocketListener requestContext = do
  let sql = "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE query = 'LISTEN websocket' ORDER BY query_start DESC LIMIT 1"
  (Right [Only True]) <- runInContext (runDB (`query_` sql)) requestContext
  return ()
