module Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.DeliverySpec where

import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Network.WebSockets
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Model.Config.WizardServerConfig
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.Common
import Specs.Api.Handler.Websocket.Common

deliverySpec requestContext =
  describe "delivery" $ do
    test_order requestContext
    test_userList requestContext
    test_ping requestContext
    test_invalidToken requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_order requestContext =
  it "WS 200 OK - replies from one sender arrive in order" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      uuids <- traverse (const generateUuid) [1 .. 3 :: Int]
      let dtos = fmap (\uuid -> setRepliesDTO {uuid = uuid} :: SetRepliesDTO) uuids
      -- WHEN:
      traverse_ (write_SetReplies c1) dtos
      -- THEN:
      traverse_ (read_SetRepliesUuid c2) dtos
      traverse_ (read_SetRepliesUuid c1) dtos
      -- AND: Close sockets
      closeSockets [s1, s2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_invalidToken requestContext =
  it "WS 401 UNAUTHORIZED - invalid token rejects the handshake" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      clearConnections requestContext editor.uuid
      -- WHEN:
      -- THEN:
      read_Rejected requestContext.serverConfig.general.serverPort (reqUrlT editor.uuid (Just "invalid-token")) 401

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_userList requestContext =
  it "WS 200 OK - user list follows connects and disconnects" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      clearConnections requestContext editor.uuid
      -- WHEN: Connect 1. user
      (c1, s1) <- createConnection requestContext (reqUrlT editor.uuid (Just reqAuthToken))
      -- THEN:
      read_SetUserList c1 0
      -- WHEN: Connect 2. user
      (c2, s2) <- createConnection requestContext (reqUrlT editor.uuid (Just reqNonAdminAuthToken))
      -- THEN:
      read_SetUserList c2 1
      read_SetUserList c1 1
      -- WHEN: Disconnect 2. user
      closeConnection c2
      -- THEN:
      read_SetUserList c1 0
      -- AND: Close sockets
      closeSockets [s1, s2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_ping requestContext =
  it "WS 200 OK - ping text is accepted silently" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      -- WHEN:
      sendTextData c1 (T.pack "ping")
      -- THEN:
      nothingWasReceivedWithin 300000 c1
      assertCountOfWebsocketConnection requestContext 2
      -- AND: Close sockets
      closeSockets [s1, s2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
write_SetReplies connection repliesDto = sendMessage connection (SetReplies_ClientKnowledgeModelEditorMessageDTO repliesDto)

read_SetRepliesUuid :: Connection -> SetRepliesDTO -> IO ()
read_SetRepliesUuid connection expDto = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO)
  let (Right (Success_ServerActionDTO (SetReplies_ServerKnowledgeModelEditorMessageDTO repliesDto))) = eResult
  repliesDto.uuid `shouldBe` expDto.uuid
