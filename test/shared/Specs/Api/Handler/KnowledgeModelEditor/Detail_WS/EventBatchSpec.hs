module Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.EventBatchSpec where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (isRight)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status (badRequest400, ok200)
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.Http.HttpRequest
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventMapper

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.Common
import Specs.Api.Handler.KnowledgeModelEditor.Event.Common
import Specs.Api.Handler.Project.Detail_WS.Common (runSimpleRequest)
import Specs.Api.Handler.Websocket.Common

eventBatchSpec requestContext =
  describe "eventBatch" $ do
    test200 requestContext
    test400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 requestContext = do
  create_test_200 "WS 200 OK" requestContext a_km1_ch4
  create_test_200 "WS 200 OK - a large event through the websocket outbox" requestContext largeChapterEvent

create_test_200 title requestContext event =
  it title $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      ((c3, s3), (c4, s4)) <- connectTestWebsocketUsers requestContext leidenKnowledgeModelEditor.uuid
      -- WHEN:
      response <- runSimpleRequest requestContext (eventsRequest editor.uuid [event])
      -- THEN:
      isRight response `shouldBe` True
      let (Right r) = response
      HC.responseStatus r `shouldBe` ok200
      let (Right resDto) = eitherDecode (HC.responseBody r) :: Either String KnowledgeModelEditorEventsDTO
      -- AND:
      read_AddedEvent c1 resDto.revision event
      read_AddedEvent c2 resDto.revision event
      nothingWasReceived c3
      nothingWasReceived c4
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test400 requestContext =
  it "WS 400 BAD REQUEST - a rejected batch is not broadcast" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      -- WHEN:
      response <- runSimpleRequest requestContext (eventsRequest editor.uuid [a_km1_ch4, a_km1_ch2_q4 {parentUuid = nonExistingUuid}])
      -- THEN:
      isRight response `shouldBe` True
      let (Right r) = response
      HC.responseStatus r `shouldBe` badRequest400
      -- AND:
      nothingWasReceived c1
      nothingWasReceived c2
      -- AND: Close sockets
      closeSockets [s1, s2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
largeChapterEvent :: KnowledgeModelEvent
largeChapterEvent =
  case a_km1_ch4.content of
    AddChapterEvent' content -> a_km1_ch4 {content = AddChapterEvent' content {text = Just (replicate 10000 'x')}}
    _ -> a_km1_ch4

eventsRequest :: U.UUID -> [KnowledgeModelEvent] -> HttpRequest
eventsRequest editorUuid events =
  HttpRequest
    { requestMethod = "POST"
    , requestUrl = "/api/knowledge-model-editors/" ++ U.toString editorUuid ++ "/events"
    , requestHeaders = M.fromList [("Authorization", "Bearer " ++ reqAuthToken), ("Content-Type", "application/json")]
    , requestBody = BSL.toStrict . encode . eventsChange . fmap toEventChangeDTO $ events
    , multipart = Nothing
    }

read_AddedEvent connection revision expEvent = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerKnowledgeModelEditorMessageDTO (AddKnowledgeModelEditorWebSocketEventDTO' eventDto)))) = eResult
  Just eventDto.uuid `shouldBe` revision
  (eventDto.event.parentUuid, eventDto.event.entityUuid, eventDto.event.content) `shouldBe` (expEvent.parentUuid, expEvent.entityUuid, expEvent.content)
