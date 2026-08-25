module Shared.Common.Util.Sentry where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException (..), fromException, someExceptionContext)
import Control.Exception.Context (displayExceptionContext)
import Control.Monad (void, when)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, modifyIORef')
import qualified Data.List as L
import Data.Maybe (isJust, mapMaybe, maybeToList)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Typeable (typeOf)
import qualified Data.UUID as U
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (defaultOnException)
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryRecord (..))
import System.TimeManager (TimeoutThread (..))

import Shared.Common.Model.Sentry.SentryEvent

sendSentryEvent :: SentryTarget -> SentryEvent -> IO ()
sendSentryEvent target event =
  when (target.enabled && not (null target.dsn)) . void . forkIO $ do
    service <- initRaven target.dsn id sendRecord stderrFallback
    register service event.logger event.level event.message (updateSentryRecord target event)

updateSentryRecord :: SentryTarget -> SentryEvent -> SentryRecord -> SentryRecord
updateSentryRecord target event record =
  record
    { srPlatform = Just "haskell"
    , srCulprit = event.culprit
    , srServerName = event.serverName
    , srRelease = Just target.release
    , srEnvironment = Just target.environment
    , srTags = HashMap.fromList event.tags
    , srExtra = HashMap.fromList event.extra
    , srInterfaces = HashMap.fromList (toFingerprintInterface event.fingerprint ++ toBreadcrumbsInterface event.breadcrumbs ++ event.interfaces)
    }

toFingerprintInterface :: [String] -> [(String, Value)]
toFingerprintInterface [] = []
toFingerprintInterface fingerprint = [("fingerprint", toJSON fingerprint)]

breadcrumbLimit :: Int
breadcrumbLimit = 50

addBreadcrumb :: IORef [SentryBreadcrumb] -> SentryBreadcrumb -> IO ()
addBreadcrumb buffer breadcrumb = modifyIORef' buffer (take breadcrumbLimit . (breadcrumb :))

toBreadcrumbsInterface :: [SentryBreadcrumb] -> [(String, Value)]
toBreadcrumbsInterface [] = []
toBreadcrumbsInterface breadcrumbs = [("breadcrumbs", object ["values" .= fmap toBreadcrumb breadcrumbs])]

toBreadcrumb :: SentryBreadcrumb -> Value
toBreadcrumb breadcrumb =
  object
    [ "timestamp" .= (realToFrac . utcTimeToPOSIXSeconds $ breadcrumb.timestamp :: Double)
    , "level" .= T.pack breadcrumb.level
    , "category" .= T.pack breadcrumb.category
    , "message" .= (T.pack . normalizeMessage $ breadcrumb.message)
    ]

toExceptionInterface :: SomeException -> (String, Value)
toExceptionInterface exception =
  ( "exception"
  , object
      [ "values"
          .= [ object
                 [ "type" .= (T.pack . getExceptionType $ exception)
                 , "value" .= (T.pack . normalizeMessage . formatException $ exception)
                 , "stacktrace" .= object ["frames" .= toStacktraceFrames exception]
                 ]
             ]
      ]
  )

toStacktraceFrames :: SomeException -> [Value]
toStacktraceFrames =
  reverse . mapMaybe toStacktraceFrame . lines . displayExceptionContext . someExceptionContext

toStacktraceFrame :: String -> Maybe Value
toStacktraceFrame line =
  case T.splitOn ", called at " (T.strip . T.pack $ line) of
    [function, rest] ->
      case T.splitOn " in " rest of
        [location, origin] -> toFrame function location origin
        _ -> Nothing
    _ -> Nothing

toFrame :: T.Text -> T.Text -> T.Text -> Maybe Value
toFrame function location origin =
  case (reverse . T.splitOn ":" $ location, T.splitOn ":" origin) of
    (column : row : file, [package, moduleName])
      | package /= "ghc-internal" ->
          Just $
            object
              [ "function" .= function
              , "filename" .= T.intercalate ":" (reverse file)
              , "lineno" .= readNumber row
              , "colno" .= readNumber column
              , "module" .= moduleName
              , "package" .= package
              , "in_app" .= T.isPrefixOf "fair-wizard" package
              ]
    _ -> Nothing

readNumber :: T.Text -> Int
readNumber text =
  case reads . T.unpack $ text of
    [(number, "")] -> number
    _ -> 0

toTraceInterface :: String -> [(String, Value)]
toTraceInterface traceUuid =
  case U.fromString traceUuid of
    Just uuid ->
      let traceId = T.filter (/= '-') . T.pack . U.toString $ uuid
       in [("contexts", object ["trace" .= object ["trace_id" .= traceId, "span_id" .= T.take 16 traceId]])]
    Nothing -> []

toUserInterface :: Maybe String -> Maybe String -> [(String, Value)]
toUserInterface Nothing Nothing = []
toUserInterface mUserUuid mEmail =
  [("user", object (maybe [] (\x -> ["id" .= T.pack x]) mUserUuid ++ maybe [] (\x -> ["email" .= T.pack x]) mEmail))]

toRequestInterface :: WAI.Request -> (String, Value)
toRequestInterface request =
  ( "request"
  , object
      [ "url" .= (T.pack . BS.unpack . WAI.rawPathInfo $ request)
      , "method" .= (T.pack . BS.unpack . WAI.requestMethod $ request)
      , "query_string" .= (T.pack . BS.unpack . WAI.rawQueryString $ request)
      , "headers" .= object ["Host" .= (T.pack . maybe "" BS.unpack . WAI.requestHeaderHost $ request)]
      ]
  )

formatException :: SomeException -> String
formatException (SomeException exception) = show exception

getExceptionType :: SomeException -> String
getExceptionType (SomeException exception) = show . typeOf $ exception

normalizeMessage :: String -> String
normalizeMessage [] = []
normalizeMessage message@(letter : rest) =
  if isJust . U.fromString . take 36 $ message
    then "<uuid>" ++ normalizeMessage (drop 36 message)
    else letter : normalizeMessage rest

normalizePath :: String -> String
normalizePath = T.unpack . T.intercalate "/" . fmap normalizePathSegment . T.splitOn "/" . T.pack

normalizePathSegment :: T.Text -> T.Text
normalizePathSegment segment =
  if isJust . U.fromString . T.unpack $ segment
    then ":uuid"
    else segment

findHeader :: String -> WAI.Request -> Maybe String
findHeader name = fmap (BS.unpack . snd) . L.find (\(headerName, _) -> CI.mk (BS.pack name) == headerName) . WAI.requestHeaders

isIgnoredException :: SomeException -> Bool
isIgnoredException exception =
  case fromException exception of
    Just TimeoutThread -> True
    Nothing -> any (`L.isInfixOf` formatException exception) ignoredExceptionMessages

ignoredExceptionMessages :: [String]
ignoredExceptionMessages =
  [ "Network.Socket.recvBuf: resource vanished"
  , "Network.Socket.sendBuf: resource vanished"
  , "Warp: Request headers did not finish transmission"
  , "Warp: Client closed connection prematurely"
  , "Thread killed by timeout manager"
  ]

sentryOnException :: SentryTarget -> (Maybe String -> [(String, Value)]) -> Maybe WAI.Request -> SomeException -> IO ()
sentryOnException target getSentryIdentity mRequest exception
  | isIgnoredException exception = return ()
  | otherwise = do
      sendSentryEvent target (toWebServerEvent getSentryIdentity mRequest exception)
      defaultOnException mRequest exception

toWebServerEvent :: (Maybe String -> [(String, Value)]) -> Maybe WAI.Request -> SomeException -> SentryEvent
toWebServerEvent _ Nothing exception =
  (toSentryEvent "webServerLogger" (normalizeMessage . formatException $ exception))
    { fingerprint = [getExceptionType exception]
    , interfaces = [toExceptionInterface exception]
    }
toWebServerEvent getSentryIdentity (Just request) exception =
  let method = BS.unpack . WAI.requestMethod $ request
      route = normalizePath . BS.unpack . WAI.rawPathInfo $ request
      mTraceUuid = findHeader "x-trace-uuid" request
   in (toSentryEvent "webServerLogger" (normalizeMessage . formatException $ exception))
        { culprit = Just (method ++ " " ++ route)
        , serverName = fmap BS.unpack . WAI.requestHeaderHost $ request
        , fingerprint = [method, route, getExceptionType exception]
        , tags = [("method", method), ("route", route)] ++ fmap (\x -> ("traceUuid", x)) (maybeToList mTraceUuid)
        , extra = [("url", String . T.pack . BS.unpack . WAI.rawPathInfo $ request)]
        , interfaces =
            [toRequestInterface request, toExceptionInterface exception]
              ++ getSentryIdentity (findHeader "Authorization" request)
              ++ concatMap toTraceInterface (maybeToList mTraceUuid)
        }
