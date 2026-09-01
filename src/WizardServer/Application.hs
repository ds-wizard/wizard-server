module WizardServer.Application where

import Control.Concurrent
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)
import System.Environment (lookupEnv, setEnv)

import Shared.Api.Middleware.WizardLoggingMiddleware
import Shared.Application
import Shared.Bootstrap.AwsAppConfig
import Shared.Bootstrap.Web
import Shared.Bootstrap.Worker
import Shared.Cache.CacheFactory
import Shared.Constant.Resource
import qualified Shared.Database.Migration.Development.Migration as DevDB
import Shared.Integration.Http.Common.HttpClientFactory (createRestrictedHttpClientManager)
import Shared.Integration.Http.Common.ServantClient
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardServerContext
import Shared.Util.Jinja (verifyJinja)
import Shared.Worker.CronWorkers
import WizardServer.Api.Sentry
import WizardServer.Api.Web
import WizardServer.Constant.ASCIIArt
import qualified WizardServer.Database.Migration.Production.Migration as ProdDB
import WizardServer.Model.Context.ContextMappers
import WizardServer.Model.Context.ServerContext
import WizardServer.Service.Config.Server.ServerConfigValidation
import WizardServer.Worker.PermanentWorkers

runApplication :: IO ()
runApplication =
  runWebServerWithWorkers
    [putStrLn asciiLogo, verifyJinja]
    serverConfigFile
    validateServerConfig
    buildInfoFile
    createServerContext
    ProdDB.migrationDefinitions
    (runRequestContextWithServerContext DevDB.runMigration)
    afterDbMigrationHook
    runWebServer
    runWorker

createServerContext :: (MonadIO m, MonadLogger m) => ServerConfig -> BuildInfoConfig -> Pool Connection -> MinioConn -> Manager -> MVar () -> m ServerContext
createServerContext serverConfig buildInfoConfig dbPool s3Client httpClientManager shutdownFlag = do
  registryClient <- liftIO $ createRegistryClient serverConfig httpClientManager
  restrictedHttpClientManager <- liftIO $ createRestrictedHttpClientManager serverConfig.logging serverConfig.httpClient.restricted.allowedHosts
  cache <- liftIO (createServerCache serverConfig)
  return ServerContext {..}

afterDbMigrationHook :: WizardServerContextType context => context -> IO ()
afterDbMigrationHook context = do
  mAwsAppConfig <- lookupEnv "AWS_APP_CONFIG"
  case mAwsAppConfig of
    Just _ -> do
      (path, poller) <- resolveConfigPath context.serverConfig'.general.integrationConfig
      setEnv "INTEGRATION_CONFIG_PATH" path
      _ <- forkIO $ poller context.shutdownFlag'
      return ()
    Nothing -> return ()

runWebServer :: ServerContext -> IO ()
runWebServer context = runWebServerFactory context getSentryIdentity loggingMiddleware webApi webServer

runWorker :: MVar () -> ServerContext -> IO ()
runWorker shutdownFlag context =
  worker runRequestContextWithServerContext runRequestContextWithServerContext'' shutdownFlag context workers permanentWorker
