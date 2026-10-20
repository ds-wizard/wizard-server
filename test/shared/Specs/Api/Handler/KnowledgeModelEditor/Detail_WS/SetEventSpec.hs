module Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.SetEventSpec where

import qualified Control.Exception as E
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (begin, close, commit, connectPostgreSQL)
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Constant.Tenant
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Service.KnowledgeModel.Editor.EditorMapper (toKnowledgeModelEditorEvent)
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.Common
import Specs.Api.Handler.KnowledgeModelEditor.Event.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

setEventSpec requestContext =
  describe "setEvent" $ do
    test200 requestContext
    test400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 requestContext = do
  create_test_200 "WS 200 OK" requestContext knowledgeModelEditorWebsocketEvent1'
  create_test_200
    "WS 200 OK if a required text is empty"
    requestContext
    (toWebsocketEvent (editChapter1 e_km1_ch1__content {title = ChangedValue "", questionUuids = NothingChanged}))
  create_test_200_locked requestContext

create_test_200_locked requestContext =
  it "WS 200 OK if a batch holds the editor, validated against the batch" $
    -- GIVEN: Prepare request
    do
      let reqDto = toWebsocketEvent (a_km1_ch1_q1 {uuid = newEntityUuid, parentUuid = a_km1_ch4.entityUuid, entityUuid = newEntityUuid})
      -- AND: Prepare database
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      -- AND: Lock the editor by a batch that adds a chapter
      batchConnection <- lockEditorInBatch requestContext editor.uuid a_km1_ch4
      -- WHEN:
      write_SetReply c1 reqDto
      -- THEN:
      (nothingWasReceived c1 >> commit batchConnection) `E.onException` close batchConnection
      read_SetReply c1 reqDto
      read_SetReply c2 reqDto
      -- AND: Close sockets
      close batchConnection
      closeSockets [s1, s2]

create_test_200 title requestContext reqDto =
  it title $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      ((c3, s3), (c4, s4)) <- connectTestWebsocketUsers requestContext leidenKnowledgeModelEditor.uuid
      -- WHEN:
      write_SetReply c1 reqDto
      -- THEN:
      read_SetReply c1 reqDto
      read_SetReply c2 reqDto
      nothingWasReceived c3
      nothingWasReceived c4
      -- AND: Find result in DB and compare with expectation state
      (Right editorFromDb) <- runInContext (findKnowledgeModelEditorByUuid editor.uuid) requestContext
      editorFromDb.squashed `shouldBe` False
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test400 requestContext =
  it "WS 400 BAD REQUEST if the event is structurally invalid, naming the rejected event" $
    -- GIVEN: Prepare request
    do
      let reqDto = toWebsocketEvent (a_km1_ch2_q4 {parentUuid = nonExistingUuid})
      -- AND: Prepare expectation
      let expError =
            ValidationError
              [_ERROR_VALIDATION__KM_EVENT_REJECTED (U.toString a_km1_ch2_q4.uuid)]
              (M.singleton "parentUuid" [_ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString nonExistingUuid) "chapter, list question or answer"])
      -- AND: Prepare database
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      -- WHEN:
      write_SetReply c1 reqDto
      -- THEN:
      read_Error c1 expError
      nothingWasReceived c2
      -- AND: Compare state in DB with expectation
      (Right events) <- runInContext (findKnowledgeModelEventsByEditorUuid editor.uuid) requestContext
      length events `shouldBe` 0
      -- AND: Close sockets
      closeSockets [s1, s2]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
lockEditorInBatch requestContext editorUuid event = do
  connection <- connectPostgreSQL (BS.pack requestContext.serverConfig.database.connectionString)
  begin connection
  let batchContext = requestContext {dbConnection = Just connection} :: RequestContext
  (Right _) <- runInContext (findKnowledgeModelEditorByUuidLocked editorUuid) batchContext
  (Right _) <- runInContext (insertKnowledgeModelEvent (toKnowledgeModelEditorEvent editorUuid defaultTenantUuid event)) batchContext
  return connection

toWebsocketEvent :: KnowledgeModelEvent -> KnowledgeModelEditorWebSocketEventDTO
toWebsocketEvent event = AddKnowledgeModelEditorWebSocketEventDTO' (knowledgeModelEditorWebsocketEvent1 {event = event})

write_SetReply connection replyDto = do
  let reqDto = SetContent_ClientKnowledgeModelEditorMessageDTO replyDto
  sendMessage connection reqDto

read_SetReply connection expReplyDto = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerKnowledgeModelEditorMessageDTO replyDto))) = eResult
  expReplyDto `shouldBe` replyDto
