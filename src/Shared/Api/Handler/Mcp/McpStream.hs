module Shared.Api.Handler.Mcp.McpStream where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.Chan
import qualified Control.Exception.Base as E
import Control.Monad (guard)
import Control.Monad.Reader (ask, asks, liftIO)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Network.HTTP.Types (Header, hCacheControl, hContentType, status200)
import Network.Wai (Response, ResponseReceived, StreamingBody, responseLBS, responseStream)
import System.Timeout (timeout)

import Shared.Api.Resource.Mcp.JsonRpc.JsonRpcResponseDTO
import Shared.Api.Resource.Mcp.JsonRpc.JsonRpcResponseJM ()
import Shared.Constant.Mcp
import Shared.Model.Context.ProgressReporter
import Shared.Model.Context.WizardRequestContext
import Shared.Service.Mcp.Server.McpServerService (internalError)
import Shared.Service.Mcp.Server.McpToolService (ContextResult, rethrowContextResult)
import Shared.Util.Error (tryError)

data McpStreamEvent
  = McpProgressEvent Value
  | McpResultEvent (ContextResult (Maybe JsonRpcResponseDTO))

respondMcp :: WizardRequestContextC s m => (Response -> IO ResponseReceived) -> Value -> m (Maybe JsonRpcResponseDTO) -> m (Maybe ResponseReceived)
respondMcp respond body action =
  case findProgressToken body of
    Just token -> respondWithProgress respond (requestIdOf body) token action
    Nothing -> traverse (respondJson respond) =<< action

respondJson :: WizardRequestContextC s m => (Response -> IO ResponseReceived) -> JsonRpcResponseDTO -> m ResponseReceived
respondJson respond response = do
  header <- traceUuidHeader
  liftIO . respond $ responseLBS status200 [(hContentType, "application/json"), header] (encode response)

respondWithProgress :: WizardRequestContextC s m => (Response -> IO ResponseReceived) -> Value -> Value -> m (Maybe JsonRpcResponseDTO) -> m (Maybe ResponseReceived)
respondWithProgress respond requestId token action = do
  context <- ask
  events <- liftIO newChan
  let reporter = ProgressReporter (\progress total message -> writeChan events (McpProgressEvent (progressNotification token progress total message)))
  let run = runRequestContextWithRequestContext (tryError action) (setProgressReporter reporter context)
  worker <- liftIO . async $ E.try run >>= writeChan events . McpResultEvent
  event <- liftIO (readChan events `E.onException` cancel worker)
  case event of
    McpResultEvent result -> traverse (respondJson respond) =<< rethrowContextResult result
    McpProgressEvent notification -> do
      header <- traceUuidHeader
      Just <$> liftIO (respond (eventStreamResponse header (streamEvents requestId events notification)) `E.finally` cancel worker)

eventStreamResponse :: Header -> StreamingBody -> Response
eventStreamResponse header = responseStream status200 [(hContentType, "text/event-stream"), (hCacheControl, "no-cache"), ("X-Accel-Buffering", "no"), header]

streamEvents :: Value -> Chan McpStreamEvent -> Value -> StreamingBody
streamEvents requestId events first write flush = send first >> loop
  where
    send message = write (eventStreamMessage message) >> flush
    loop = do
      mEvent <- timeout mcpKeepaliveInterval (readChan events)
      case mEvent of
        Nothing -> write (BB.string7 ": keepalive\n\n") >> flush >> loop
        Just (McpProgressEvent notification) -> send notification >> loop
        Just (McpResultEvent result) -> send (streamedResult requestId result)

streamedResult :: Value -> ContextResult (Maybe JsonRpcResponseDTO) -> Value
streamedResult _ (Right (Right (Right (Just response)))) = toJSON response
streamedResult requestId _ = toJSON (JsonRpcResponseDTO requestId (Left internalError))

eventStreamMessage :: Value -> BB.Builder
eventStreamMessage message = BB.string7 "event: message\ndata: " <> BB.lazyByteString (encode message) <> BB.string7 "\n\n"

progressNotification :: Value -> Double -> Maybe Double -> String -> Value
progressNotification token progress total message =
  object
    [ "jsonrpc" .= ("2.0" :: String)
    , "method" .= ("notifications/progress" :: String)
    , "params" .= object (["progressToken" .= token, "progress" .= progress, "message" .= message] ++ ["total" .= value | Just value <- [total]])
    ]

findProgressToken :: Value -> Maybe Value
findProgressToken = parseMaybe $ withObject "request" $ \o -> do
  method <- o .: "method"
  guard (method == ("tools/call" :: String))
  params <- o .: "params"
  meta <- params .: "_meta"
  token <- meta .: "progressToken"
  guard (token /= Null)
  return token

requestIdOf :: Value -> Value
requestIdOf body = fromMaybe Null (parseMaybe (withObject "request" (.: "id")) body)

traceUuidHeader :: WizardRequestContextC s m => m Header
traceUuidHeader = do
  traceUuid <- asks (.traceUuid')
  return ("x-trace-uuid", BS.pack (U.toString traceUuid))
