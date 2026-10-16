module Shared.Api.Handler.Websocket where

import Control.Concurrent.Async (Async, waitCatch, waitCatchSTM, withAsync)
import Control.Concurrent.STM
import qualified Control.Exception.Base as E
import Control.Monad (void, when)
import Control.Monad.Except (MonadError, catchError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (traverse_)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import qualified Data.UUID as U
import Data.Word (Word16)
import Network.WebSockets
import Network.WebSockets.Connection (PendingConnection (..))
import Servant (ServerError (..))
import System.Timeout (timeout)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Websocket
import Shared.Localization.Messages.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Util.Exception
import Shared.Util.Logger
import Shared.Util.Uuid
import Shared.Util.Websocket

rejectOnError :: (MonadError ServerError sm, MonadIO sm) => PendingConnection -> sm () -> sm ()
rejectOnError pendingConnection action =
  catchError action $ \error ->
    liftIO $
      rejectRequestWith
        pendingConnection
        defaultRejectRequest {rejectCode = errHTTPCode error, rejectMessage = BS.pack (errReasonPhrase error), rejectBody = BSL.toStrict (errBody error)}

runWebsocket
  :: forall s m reqDto
   . (WizardRequestContextC s m, FromJSON reqDto)
  => PendingConnection
  -> (U.UUID -> WebsocketOutbound -> m ())
  -> (U.UUID -> m ())
  -> (U.UUID -> reqDto -> m ())
  -> m ()
runWebsocket pendingConnection putUserOnline deleteUser handleAction =
  runWebsocketWith pendingConnection putUserOnline deleteUser (\connectionUuid outbound -> handleRawData connectionUuid outbound handleAction)

runWebsocketWith
  :: forall s m
   . WizardRequestContextC s m
  => PendingConnection
  -> (U.UUID -> WebsocketOutbound -> m ())
  -> (U.UUID -> m ())
  -> (U.UUID -> WebsocketOutbound -> BSL.ByteString -> m ())
  -> m ()
runWebsocketWith pendingConnection putUserOnline deleteUser handleData = do
  connection <- liftIO $ acceptRequest pendingConnection {pendingOptions = websocketOptions}
  connectionUuid <- liftIO generateUuid
  logInfoI _CMP_API (f' "[C:%s] New websocket client" [U.toString connectionUuid])
  outbound <- liftIO createOutbound
  context <- ask
  let run :: m a -> IO (Either String a)
      run action = runRequestContextWithRequestContext @s @m action context
  let receive = receiveLoop connection connectionUuid outbound (handleData connectionUuid outbound)
  let lifetime = do
        connected <- run (admitConnection outbound (putUserOnline connectionUuid outbound))
        withPingThread connection websocketPingIntervalSeconds (return ()) (runConnection connection outbound (when (connected == Right True) . void $ run receive))
  let cleanup = run (deleteUser connectionUuid) `E.finally` atomically (void (tryPutTMVar outbound.finished ()))
  liftIO $ lifetime `E.finally` cleanup
  logInfoI _CMP_API (f' "[C:%s] Websocket client disconnected" [U.toString connectionUuid])

websocketOptions :: ConnectionOptions
websocketOptions =
  defaultConnectionOptions
    { connectionFramePayloadSizeLimit = SizeLimit websocketInboundMessageSizeLimit
    , connectionMessageDataSizeLimit = SizeLimit websocketInboundMessageSizeLimit
    }

admitConnection :: WizardRequestContextC s m => WebsocketOutbound -> m () -> m Bool
admitConnection outbound putUserOnline = do
  result <- runGuarded outbound (catchError (putUserOnline >> return True) (rejectConnection outbound))
  return (result == Just True)

rejectConnection :: WizardRequestContextC s m => WebsocketOutbound -> AppError -> m Bool
rejectConnection outbound error = do
  closeOutbound outbound 1008 (Just error)
  return False

runGuarded :: forall s m a. WizardRequestContextC s m => WebsocketOutbound -> m a -> m (Maybe a)
runGuarded outbound action = do
  context <- ask
  result <- liftIO . E.uninterruptibleMask_ . tryNonAsync $ runRequestContextWithRequestContext @s @m action context
  case result of
    Right (Right value) -> return (Just value)
    Right (Left _) -> return Nothing
    Left exception -> do
      logErrorI _CMP_API ("Websocket action failed: " ++ show exception)
      closeOutbound outbound 1011 (Just (GeneralServerError (show exception)))
      return Nothing

runConnection :: Connection -> WebsocketOutbound -> IO () -> IO ()
runConnection connection outbound receive =
  withAsync (runWriter connection outbound) $ \writer ->
    withAsync (receive `E.finally` closeOutbound outbound 1000 Nothing) $ \_ -> void (waitCatch writer)

runWriter :: Connection -> WebsocketOutbound -> IO ()
runWriter connection outbound =
  withAsync (sendLoop connection outbound) $ \sender -> do
    signal <- atomically ((Right <$> readTMVar outbound.closeSignal) `orElse` (Left <$> waitCatchSTM sender))
    traverse_ (sendClose' sender) signal
  where
    sendClose' :: Async () -> (Word16, Maybe BSL.ByteString) -> IO ()
    sendClose' sender (code, mPayload) = do
      sent <- timeout websocketCloseTimeoutMicros (waitCatch sender)
      when (isJust sent) . void . timeout websocketCloseTimeoutMicros . tryNonAsync $ do
        traverse_ (sendTextData connection) mPayload
        sendCloseCode connection code BSL.empty

sendLoop :: Connection -> WebsocketOutbound -> IO ()
sendLoop connection outbound = do
  next <- atomically ((Nothing <$ readTMVar outbound.closeSignal) `orElse` (Just <$> readTBQueue outbound.queue))
  traverse_ (\payload -> sendTextData connection payload >> sendLoop connection outbound) next

receiveLoop :: WizardRequestContextC s m => Connection -> U.UUID -> WebsocketOutbound -> (BSL.ByteString -> m ()) -> m ()
receiveLoop connection connectionUuid outbound handleData = do
  logWS connectionUuid "Waiting for messages..."
  eRawData <- liftIO $ E.try (receiveData connection)
  case eRawData of
    Right rawData -> do
      logWS connectionUuid "New message received"
      handled <- runGuarded outbound (handleData rawData)
      when (isJust handled) (receiveLoop connection connectionUuid outbound handleData)
    Left (CloseRequest code message) ->
      logWS connectionUuid (f' "Request to close websocket (code: %s, msg: '%s')" [show code, show message])
    Left ConnectionClosed -> logWS connectionUuid "Connection closed"
    Left (ParseException message) -> do
      logWS connectionUuid ("Parse Exception: " ++ message)
      closeOutbound outbound (if "exceeded limit" `isInfixOf` message then 1009 else 1002) Nothing
    Left (UnicodeException message) -> logWS connectionUuid ("Unicode Exception: " ++ message)

handleRawData
  :: (WizardRequestContextC s m, FromJSON reqDto)
  => U.UUID
  -> WebsocketOutbound
  -> (U.UUID -> reqDto -> m ())
  -> BSL.ByteString
  -> m ()
handleRawData connectionUuid outbound handleAction rawData =
  case eitherDecode rawData of
    Right action -> catchError (handleAction connectionUuid action) (sendError outbound connectionUuid)
    Left error
      | "ping" `isInfixOf` error -> return ()
      | otherwise -> do
          logWS connectionUuid ("Deserialization failed: " ++ error)
          sendError outbound connectionUuid (UserError _ERROR_API_WEBSOCKET__DESERIALIZATION_FAILED)

sendError :: WizardRequestContextC s m => WebsocketOutbound -> U.UUID -> AppError -> m ()
sendError outbound connectionUuid error = enqueueOutbound outbound connectionUuid (encodeError error)
