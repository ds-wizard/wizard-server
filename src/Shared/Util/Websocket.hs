module Shared.Util.Websocket where

import Control.Concurrent.STM
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.UUID as U
import Data.Word (Word16)

import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Api.Resource.Websocket.WebsocketActionJM ()
import Shared.Constant.Websocket
import Shared.Localization.Messages.Public
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Util.Logger

createOutbound :: IO WebsocketOutbound
createOutbound = WebsocketOutbound <$> newTBQueueIO (fromIntegral websocketOutboundQueueSize) <*> newEmptyTMVarIO <*> newEmptyTMVarIO

enqueueMessage :: (WizardRequestContextC s m, ToJSON a) => LocalWebsocketConnection -> a -> m ()
enqueueMessage connection content = enqueueOutbound connection.outbound connection.connectionUuid (encode content)

enqueueOutbound :: WizardRequestContextC s m => WebsocketOutbound -> U.UUID -> BSL.ByteString -> m ()
enqueueOutbound outbound connectionUuid payload = do
  accepted <- liftIO . atomically $ do
    full <- isFullTBQueue outbound.queue
    unless full (writeTBQueue outbound.queue payload)
    return (not full)
  unless accepted $ do
    logInfoI _CMP_API (f' "[C:%s] Outbound queue is full, closing the connection" [U.toString connectionUuid])
    closeOutbound outbound 1008 (Just . UserError $ _ERROR_API_WEBSOCKET__QUEUE_OVERFLOW)

closeConnection :: MonadIO m => LocalWebsocketConnection -> Word16 -> Maybe AppError -> m ()
closeConnection connection = closeOutbound connection.outbound

closeOutbound :: MonadIO m => WebsocketOutbound -> Word16 -> Maybe AppError -> m ()
closeOutbound outbound code mError =
  liftIO . atomically . void $ tryPutTMVar outbound.closeSignal (code, fmap encodeError mError)

encodeError :: AppError -> BSL.ByteString
encodeError = encode . Error_ServerActionDTO

logWS :: WizardRequestContextC s m => U.UUID -> String -> m ()
logWS connectionUuid message = do
  serverConfig <- asks (.serverConfig')
  when
    serverConfig.logging.websocketDebug
    (logInfoI _CMP_API (f' "[C:%s] %s" [U.toString connectionUuid, message]))
