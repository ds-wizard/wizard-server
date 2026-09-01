module Shared.Model.Context.RequestContext where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.IORef (IORef)
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import GHC.Records
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.ServerConfig
import Shared.Model.Error.Error
import Shared.Model.Sentry.SentryEvent
import Shared.Service.Acl.AclService

class
  ( HasField "dbPool'" context (Pool Connection)
  , HasField "dbConnection'" context (Maybe Connection)
  , HasField "s3Client'" context MinioConn
  , HasField "identity'" context (Maybe String)
  , HasField "traceUuid'" context U.UUID
  , HasField "breadcrumbs'" context (IORef [SentryBreadcrumb])
  , HasField "tenantUuid'" context U.UUID
  , HasField "serverConfig'" context sc
  , HasField "database'" sc ServerConfigDatabase
  , HasField "cloud'" sc ServerConfigCloud
  , HasField "s3'" sc ServerConfigS3
  , HasField "persistentCommand'" sc ServerConfigPersistentCommand
  , HasField "sentry'" sc ServerConfigSentry
  , HasField "logging'" sc ServerConfigLogging
  , HasField "aws'" sc ServerConfigAws
  , HasField "buildInfoConfig'" context BuildInfoConfig
  , HasField "httpClientManager'" context Manager
  ) =>
  RequestContextType context sc

class
  ( MonadLogger m
  , MonadIO m
  , MonadError AppError m
  , MonadReader context m
  , RequestContextType context sc
  , AclContext m
  ) =>
  RequestContextC context sc m
