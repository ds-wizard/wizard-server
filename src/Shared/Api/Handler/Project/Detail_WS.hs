module Shared.Api.Handler.Project.Detail_WS where

import qualified Data.UUID as U
import Network.WebSockets
import Servant
import Servant.API.WebSocket

import Shared.Api.Handler.Websocket
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.ProjectMessageJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Util.Websocket

type Detail_WS =
  Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "websocket"
    :> QueryParam "Authorization" String
    :> WebSocketPending

detail_WS :: WizardHandlerC s sm r rm => Maybe String -> U.UUID -> Maybe String -> PendingConnection -> sm ()
detail_WS mServerUrl projectUuid mTokenHeader pendingConnection =
  rejectOnError pendingConnection . getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $
      runWebsocket pendingConnection (putUserOnline projectUuid) (deleteUser projectUuid) handleAction
  where
    handleAction connectionUuid (SetContent_ClientProjectMessageDTO reqDto) = do
      logWS connectionUuid "SetContent"
      setContent projectUuid connectionUuid reqDto
