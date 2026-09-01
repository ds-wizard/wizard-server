module Shared.Model.Context.ServerContext where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import GHC.Records
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)
import Servant (ServerError)

import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.ServerConfig

class
  ( HasField "dbPool'" context (Pool Connection)
  , HasField "s3Client'" context MinioConn
  , HasField "serverConfig'" context sc
  , HasField "cloud'" sc ServerConfigCloud
  , HasField "s3'" sc ServerConfigS3
  , HasField "sentry'" sc ServerConfigSentry
  , HasField "serverPort'" sc Int
  , HasField "environment'" sc String
  , HasField "logging'" sc ServerConfigLogging
  , HasField "aws'" sc ServerConfigAws
  , HasField "buildInfoConfig'" context BuildInfoConfig
  , HasField "httpClientManager'" context Manager
  ) =>
  ServerContextType context sc

class
  ( MonadLogger m
  , MonadIO m
  , MonadError ServerError m
  , MonadReader context m
  , ServerContextType context sc
  ) =>
  ServerContextC context sc m
