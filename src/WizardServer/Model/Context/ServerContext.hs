module WizardServer.Model.Context.ServerContext where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import GHC.Records
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)
import Servant (ServerError)
import Servant.Client (ClientEnv)
import Shared.Model.Cache.ServerCache
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.ServerContext
import Shared.Model.Context.WizardServerContext

data ServerContext = ServerContext
  { serverConfig :: ServerConfig
  , buildInfoConfig :: BuildInfoConfig
  , dbPool :: Pool Connection
  , s3Client :: MinioConn
  , httpClientManager :: Manager
  , restrictedHttpClientManager :: Manager
  , registryClient :: ClientEnv
  , shutdownFlag :: MVar ()
  , cache :: ServerCache
  }

newtype ServerContextM a = ServerContextM
  { runServerContextM :: ReaderT ServerContext (LoggingT (ExceptT ServerError IO)) a
  }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader ServerContext, MonadError ServerError, MonadLogger)

instance ServerContextType ServerContext ServerConfig

instance ServerContextC ServerContext ServerConfig ServerContextM

instance WizardServerContextType ServerContext

instance WizardServerContextC ServerContext ServerContextM

instance HasField "serverConfig'" ServerContext ServerConfig where
  getField = (.serverConfig)

instance HasField "dbPool'" ServerContext (Pool Connection) where
  getField = (.dbPool)

instance HasField "s3Client'" ServerContext MinioConn where
  getField = (.s3Client)

instance HasField "httpClientManager'" ServerContext Manager where
  getField = (.httpClientManager)

instance HasField "restrictedHttpClientManager'" ServerContext Manager where
  getField = (.restrictedHttpClientManager)

instance HasField "buildInfoConfig'" ServerContext BuildInfoConfig where
  getField = (.buildInfoConfig)

instance HasField "cache'" ServerContext ServerCache where
  getField = (.cache)

instance HasField "registryClient'" ServerContext ClientEnv where
  getField = (.registryClient)

instance HasField "shutdownFlag'" ServerContext (MVar ()) where
  getField = (.shutdownFlag)
