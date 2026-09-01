module Shared.Bootstrap.Web where

import Control.Exception (SomeException)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (Value)
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setOnException, setPort)
import Network.Wai.Middleware.Servant.Errors (errorMw)
import Servant

import Shared.Api.Middleware.CORSMiddleware
import Shared.Api.Middleware.ContentTypeMiddleware
import Shared.Api.Middleware.OptionsMiddleware
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.ServerContext
import Shared.Service.Sentry.SentryService
import Shared.Util.Logger
import Shared.Util.Sentry

runWebServerFactory
  :: (ServerContextType context sc, HasServer api '[])
  => context
  -> (Maybe String -> [(String, Value)])
  -> (String -> WAI.Middleware)
  -> Proxy api
  -> (context -> Server api)
  -> IO ()
runWebServerFactory context getSentryIdentity loggingMiddleware webApi webServer =
  runWebApplicationFactory context getSentryIdentity loggingMiddleware (serve webApi (webServer context))

-- Runs a prebuilt WAI application; the context only supplies port, environment and Sentry.
-- Used when one Warp listener serves an application composed from several contexts.
runWebApplicationFactory
  :: ServerContextType context sc
  => context
  -> (Maybe String -> [(String, Value)])
  -> (String -> WAI.Middleware)
  -> Application
  -> IO ()
runWebApplicationFactory context getSentryIdentity loggingMiddleware application = do
  let webPort = context.serverConfig'.serverPort'
  let env = context.serverConfig'.environment'
  sentryHandler <- createSentryHandler context getSentryIdentity
  let settings = setPort webPort . setOnException sentryHandler $ defaultSettings
  runSettings settings (runMiddleware env loggingMiddleware application)

convert
  :: ServerContextType context sc
  => context
  -> (function -> ReaderT context (LoggingT (ExceptT ServerError IO)) a)
  -> function
  -> Handler a
convert serverContext runServerContextM function =
  let loggingLevel = serverContext.serverConfig'.logging'.level
   in Handler . runLogging loggingLevel $ runReaderT (runServerContextM function) serverContext

runMiddleware :: String -> (String -> WAI.Middleware) -> Application -> Application
runMiddleware env loggingMiddleware =
  contentTypeMiddleware
    . corsMiddleware
    . errorMw @JSON @'["message", "status"]
    . loggingMiddleware env
    . optionsMiddleware

createSentryHandler :: ServerContextType context sc => context -> (Maybe String -> [(String, Value)]) -> IO (Maybe WAI.Request -> SomeException -> IO ())
createSentryHandler context getSentryIdentity = return $ sentryOnException (toSentryTarget context) getSentryIdentity
