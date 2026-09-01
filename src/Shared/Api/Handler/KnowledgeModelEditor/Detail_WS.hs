module Shared.Api.Handler.KnowledgeModelEditor.Detail_WS where

import Control.Monad.Except (catchError)
import qualified Data.UUID as U
import Network.WebSockets
import Servant
import Servant.API.WebSocket
import Prelude hiding (log)

import Shared.Api.Handler.Websocket
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Shared.Util.Websocket

type Detail_WS =
  Header "Host" String
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> "websocket"
    :> QueryParam "Authorization" String
    :> WebSocket

detail_WS :: WizardHandlerC s sm r rm => Maybe String -> U.UUID -> Maybe String -> Connection -> sm ()
detail_WS mServerUrl editorUuid mTokenHeader connection =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ do
      connectionUuid <- initConnection
      catchError
        (putUserOnline editorUuid connectionUuid connection)
        (sendError connectionUuid connection (U.toString editorUuid) disconnectUser)
      handleMessage editorUuid connectionUuid connection

handleMessage :: WizardRequestContextC r rm => U.UUID -> U.UUID -> Connection -> rm ()
handleMessage editorUuid connectionUuid connection =
  handleWebsocketMessage (U.toString editorUuid) connectionUuid connection handleClose disconnectUser handleAction continue
  where
    continue = handleMessage editorUuid connectionUuid connection
    -- ------------------------------------------------------------------------------------
    handleClose = deleteUser editorUuid connectionUuid
    -- ------------------------------------------------------------------------------------
    handleAction (SetContent_ClientKnowledgeModelEditorMessageDTO reqDto) = do
      log connectionUuid "SetContent"
      setContent editorUuid connectionUuid reqDto
      handleMessage editorUuid connectionUuid connection
    handleAction (SetReplies_ClientKnowledgeModelEditorMessageDTO reqDto) = do
      log connectionUuid "SetReplies"
      setReplies editorUuid connectionUuid reqDto
      handleMessage editorUuid connectionUuid connection
