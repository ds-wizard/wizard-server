module Shared.Application where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Reader (liftIO)
import Data.Foldable (forM_)
import System.Exit
import System.IO

import Shared.Bootstrap.AwsAppConfig
import Shared.Bootstrap.Config
import Shared.Bootstrap.DatabaseMigration
import Shared.Bootstrap.HttpClient
import Shared.Bootstrap.Postgres
import Shared.Bootstrap.S3
import Shared.Constant.Component
import Shared.Model.Config.ServerConfig
import Shared.Service.Config.BuildInfo.BuildInfoConfigService
import Shared.Service.Config.Server.ServerConfigService
import Shared.Util.Logger

runWebServerWithWorkers
  beforeLoadActions
  serverConfigFile
  validateServerConfig
  buildInfoFile
  createServerContext
  prodDBMigrations
  runDevDBMigrations
  afterDbMigrationHook
  runWebServer
  worker =
    do
      hSetBuffering stdout LineBuffering
      sequence_ beforeLoadActions
      shutdownFlag <- newEmptyMVar
      (configBytes, pollForChanges) <- resolveConfigBytes serverConfigFile
      configValue <- loadConfigValue serverConfigFile configBytes
      _ <- forkIO $ pollForChanges shutdownFlag
      serverContext <-
        setupApp
          serverConfigFile
          configValue
          validateServerConfig
          buildInfoFile
          createServerContext
          prodDBMigrations
          runDevDBMigrations
          afterDbMigrationHook
          shutdownFlag
      race_ (takeMVar shutdownFlag) (concurrently (runWebServer serverContext) (worker shutdownFlag serverContext))

setupApp
  configLabel
  configValue
  validateServerConfig
  buildInfoFile
  createServerContext
  prodDBMigrations
  runDevDBMigrations
  afterDbMigrationHook
  shutdownFlag =
    do
      serverConfig <- loadConfigWith configLabel configValue (getServerConfigFromValue validateServerConfig)
      buildInfoConfig <- loadConfig buildInfoFile getBuildInfoConfig
      runLogging serverConfig.logging.level $ do
        logInfo _CMP_ENVIRONMENT $ "set to " ++ serverConfig.general.environment
        dbPool <- connectPostgresDB serverConfig.logging serverConfig.database
        httpClientManager <- setupHttpClientManager serverConfig.logging
        s3Client <- setupS3Client serverConfig.s3 httpClientManager
        serverContext <- createServerContext serverConfig buildInfoConfig dbPool s3Client httpClientManager shutdownFlag
        result <- liftIO $ runDBMigration serverContext prodDBMigrations runDevDBMigrations
        forM_ result (liftIO . die)
        liftIO $ afterDbMigrationHook serverContext
        return serverContext
