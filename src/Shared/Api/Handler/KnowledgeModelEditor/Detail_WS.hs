module Shared.Api.Handler.KnowledgeModelEditor.Detail_WS where

import qualified Data.UUID as U
import Network.WebSockets
import Servant
import Servant.API.WebSocket

import Shared.Api.Handler.Websocket
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Shared.Util.Websocket

type Detail_WS =
  Header "Host" String
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> "websocket"
    :> QueryParam "Authorization" String
    :> WebSocketPending

detail_WS :: WizardHandlerC s sm r rm => Maybe String -> U.UUID -> Maybe String -> PendingConnection -> sm ()
detail_WS mServerUrl editorUuid mTokenHeader pendingConnection =
  rejectOnError pendingConnection . getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $
      runWebsocket pendingConnection (putUserOnline editorUuid) (deleteUser editorUuid) handleAction
  where
    handleAction connectionUuid (SetContent_ClientKnowledgeModelEditorMessageDTO reqDto) = do
      logWS connectionUuid "SetContent"
      setContent editorUuid connectionUuid reqDto
    handleAction connectionUuid (SetReplies_ClientKnowledgeModelEditorMessageDTO reqDto) = do
      logWS connectionUuid "SetReplies"
      setReplies editorUuid connectionUuid reqDto
