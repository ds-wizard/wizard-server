module Shared.Api.Handler.Project.Detail_WS where

import Control.Monad.Except (catchError)
import qualified Data.UUID as U
import Network.WebSockets
import Servant
import Servant.API.WebSocket
import Prelude hiding (log)

import Shared.Api.Handler.Websocket
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.ProjectMessageJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Util.Websocket

type Detail_WS =
  Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "websocket"
    :> QueryParam "Authorization" String
    :> WebSocket

detail_WS :: WizardHandlerC s sm r rm => Maybe String -> U.UUID -> Maybe String -> Connection -> sm ()
detail_WS mServerUrl projectUuid mTokenHeader connection =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ do
      connectionUuid <- initConnection
      catchError
        (putUserOnline projectUuid connectionUuid connection)
        (sendError connectionUuid connection (U.toString projectUuid) disconnectUser)
      handleMessage projectUuid connectionUuid connection

handleMessage :: WizardRequestContextC r rm => U.UUID -> U.UUID -> Connection -> rm ()
handleMessage projectUuid connectionUuid connection =
  handleWebsocketMessage (U.toString projectUuid) connectionUuid connection handleClose disconnectUser handleAction continue
  where
    continue = handleMessage projectUuid connectionUuid connection
    -- ------------------------------------------------------------------------------------
    handleClose = deleteUser projectUuid connectionUuid
    -- ------------------------------------------------------------------------------------
    handleAction (SetContent_ClientProjectMessageDTO reqDto) = do
      log connectionUuid "SetContent"
      setContent projectUuid connectionUuid reqDto
      handleMessage projectUuid connectionUuid connection
