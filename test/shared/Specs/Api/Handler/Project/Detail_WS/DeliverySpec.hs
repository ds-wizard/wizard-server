module Specs.Api.Handler.Project.Detail_WS.DeliverySpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import qualified Control.Exception as E
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.UUID as U
import Data.Word (Word16)
import Network.WebSockets
import Test.Hspec hiding (expectationFailure, shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.Project.Event.ProjectEventDTO
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectReply
import Shared.Util.Uuid

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.Project.Detail_WS.Common
import Specs.Api.Handler.Websocket.Common

deliverySpec requestContext =
  describe "delivery" $ do
    test_order requestContext
    test_slowClient requestContext
    test_overflowFrame requestContext
    test_actionFailure requestContext
    test_ping requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_order requestContext =
  it "WS 200 OK - events from one sender arrive in order" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project.uuid
      events <- traverse (setReplyEvent project.uuid . show) [1 .. 3 :: Int]
      -- WHEN:
      traverse_ (write_SetReply c1) events
      -- THEN:
      traverse_ (read_SetReplyValue c2) events
      traverse_ (read_SetReplyValue c1) events
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_slowClient requestContext =
  it "WS 200 OK - slow client is closed and the others still receive everything" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      -- AND: Connect to websocket (c2 and c3 never read)
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project.uuid
      events <- traverse (\i -> setReplyEvent project.uuid (show i ++ replicate 65536 'x')) [1 .. 400 :: Int]
      -- WHEN:
      withAsync (traverse_ (write_SetReply c1) events) $ \sender -> do
        -- THEN:
        traverse_ (read_SetReplyValueAmidUserLists c1) events
        wait sender
      -- AND: Slow clients are gone
      assertCountOfWebsocketConnectionEventually requestContext 1
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_overflowFrame requestContext =
  it "WS 200 OK - a client that falls behind gets the overflow error and close 1008" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      -- AND: Connect to websocket (c2 reads slowly, c3 never reads)
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project.uuid
      events <- traverse (\i -> setReplyEvent project.uuid (show i ++ replicate 65536 'x')) [1 .. 800 :: Int]
      -- WHEN:
      withAsync (readSlowlyUntilClosed c2) $ \slowReader -> do
        withAsync (traverse_ (write_SetReply c1) events) $ \sender -> do
          traverse_ (read_SetReplyValueAmidUserLists c1) events
          wait sender
        -- THEN:
        (lastMessage, closeCode) <- wait slowReader
        fmap eitherDecode lastMessage `shouldBe` Just (Right (Error_ServerActionDTO (UserError _ERROR_API_WEBSOCKET__QUEUE_OVERFLOW)))
        closeCode `shouldBe` Just 1008
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_actionFailure requestContext =
  it "WS 500 INTERNAL SERVER ERROR - an action failing in the database sends an error and closes 1011" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project.uuid
      -- AND: Prepare a comment for a thread that does not exist
      let reqDto = AddCommentEventChangeDTO' acche_rQ2_t1_1 {newThread = False}
      -- WHEN:
      sendMessage c1 (SetContent_ClientProjectMessageDTO reqDto)
      -- THEN:
      read_Error c1 (GeneralServerError "")
      read_CloseCode c1 1011
      read_SetUserList c2 1
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_ping requestContext =
  it "WS 200 OK - ping text is accepted silently" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project.uuid
      -- WHEN:
      sendTextData c1 (T.pack "ping")
      -- THEN:
      nothingWasReceivedWithin 300000 c1
      assertCountOfWebsocketConnection requestContext 3
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
setReplyEvent :: U.UUID -> String -> IO SetReplyEventChangeDTO
setReplyEvent projectUuid value = do
  uuid <- generateUuid
  return SetReplyEventChangeDTO {uuid = uuid, path = (sre_rQ1Updated projectUuid).path, value = StringReply value}

write_SetReply :: Connection -> SetReplyEventChangeDTO -> IO ()
write_SetReply connection replyDto = sendMessage connection (SetContent_ClientProjectMessageDTO (SetReplyEventChangeDTO' replyDto))

read_SetReplyValue :: Connection -> SetReplyEventChangeDTO -> IO ()
read_SetReplyValue connection expDto = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerProjectMessageDTO (SetReplyEventDTO' replyDto)))) = eResult
  replyDto.value `shouldBe` expDto.value

read_SetReplyValueAmidUserLists :: Connection -> SetReplyEventChangeDTO -> IO ()
read_SetReplyValueAmidUserLists connection expDto = do
  resDto <- receiveDataWithin connection
  case eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO) of
    Right (Success_ServerActionDTO (SetUserList_ServerProjectMessageDTO _)) -> read_SetReplyValueAmidUserLists connection expDto
    Right (Success_ServerActionDTO (SetContent_ServerProjectMessageDTO (SetReplyEventDTO' replyDto))) -> replyDto.value `shouldBe` expDto.value
    other -> expectationFailure ("Expected a reply, got: " ++ show other)

readSlowlyUntilClosed :: Connection -> IO (Maybe BSL.ByteString, Maybe Word16)
readSlowlyUntilClosed connection = go Nothing
  where
    go lastMessage = do
      threadDelay 20000
      result <- E.try (receiveDataMessageWithin connection)
      case result of
        Right (Text message _) -> go (Just message)
        Right (Binary message) -> go (Just message)
        Left (CloseRequest code _) -> return (lastMessage, Just code)
        Left _ -> return (lastMessage, Nothing)
