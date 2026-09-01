module WizardServer.Model.Context.RequestContext where

import Control.Concurrent.MVar (MVar)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.IORef (IORef)
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import GHC.Records
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)
import Servant.Client (ClientEnv)
import Shared.Api.Resource.User.UserDTO
import Shared.Constant.Component
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.User.Public
import Shared.Model.Cache.ServerCache
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.RequestContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Sentry.SentryEvent
import Shared.Model.User.RoleSimple
import Shared.Service.Acl.AclService
import Shared.Util.Logger

data RequestContext = RequestContext
  { serverConfig :: ServerConfig
  , buildInfoConfig :: BuildInfoConfig
  , dbPool :: Pool Connection
  , dbConnection :: Maybe Connection
  , s3Client :: MinioConn
  , httpClientManager :: Manager
  , restrictedHttpClientManager :: Manager
  , registryClient :: ClientEnv
  , traceUuid :: U.UUID
  , breadcrumbs :: IORef [SentryBreadcrumb]
  , currentTenantUuid :: U.UUID
  , currentUser :: Maybe UserDTO
  , shutdownFlag :: MVar ()
  , cache :: ServerCache
  }

newtype RequestContextM a = RequestContextM
  { runRequestContextM :: ReaderT RequestContext (LoggingT (ExceptT AppError IO)) a
  }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader RequestContext, MonadError AppError, MonadLogger)

instance RequestContextType RequestContext ServerConfig

instance RequestContextC RequestContext ServerConfig RequestContextM

instance WizardRequestContextType RequestContext where
  setTenantUuid tenantUuid context = context {currentTenantUuid = tenantUuid}
  setCurrentUser user context = context {currentUser = user}
  setTraceUuid uuid context = context {traceUuid = uuid}
  setDbConnection connection context = context {dbConnection = connection}

instance HasField "serverConfig'" RequestContext ServerConfig where
  getField = (.serverConfig)

instance HasField "dbPool'" RequestContext (Pool Connection) where
  getField = (.dbPool)

instance HasField "dbConnection'" RequestContext (Maybe Connection) where
  getField = (.dbConnection)

instance HasField "s3Client'" RequestContext MinioConn where
  getField = (.s3Client)

instance HasField "httpClientManager'" RequestContext Manager where
  getField = (.httpClientManager)

instance HasField "restrictedHttpClientManager'" RequestContext Manager where
  getField = (.restrictedHttpClientManager)

instance HasField "buildInfoConfig'" RequestContext BuildInfoConfig where
  getField = (.buildInfoConfig)

instance HasField "identity'" RequestContext (Maybe String) where
  getField entity = fmap (U.toString . (.uuid)) entity.currentUser

instance HasField "identityEmail'" RequestContext (Maybe String) where
  getField entity = fmap (.email) entity.currentUser

instance HasField "traceUuid'" RequestContext U.UUID where
  getField = (.traceUuid)

instance HasField "breadcrumbs'" RequestContext (IORef [SentryBreadcrumb]) where
  getField = (.breadcrumbs)

instance HasField "tenantUuid'" RequestContext U.UUID where
  getField = (.currentTenantUuid)

instance HasField "cache'" RequestContext ServerCache where
  getField = (.cache)

instance HasField "currentUser'" RequestContext (Maybe UserDTO) where
  getField = (.currentUser)

instance HasField "registryClient'" RequestContext ClientEnv where
  getField = (.registryClient)

instance HasField "shutdownFlag'" RequestContext (MVar ()) where
  getField = (.shutdownFlag)

instance WizardRequestContextC RequestContext RequestContextM where
  runRequestContextWithRequestContext function requestContext = do
    let loggingLevel = requestContext.serverConfig.logging.level
    eResult <- runExceptT . runLogging loggingLevel $ runReaderT (runRequestContextM function) requestContext
    case eResult of
      Right result -> return . Right $ result
      Left error ->
        runLogging loggingLevel $ do
          logError _CMP_SERVER ("Caught error: " ++ show error)
          return . Left $ show error

instance AclContext RequestContextM where
  checkPermission perm = do
    mCurrentUser <- asks currentUser
    case mCurrentUser of
      Nothing -> throwError . ForbiddenError $ _ERROR_SERVICE_USER__MISSING_USER
      Just user ->
        unless
          (perm `elem` user.role.permissions)
          (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ perm))
  checkPermissionsAny perms = do
    mCurrentUser <- asks currentUser
    case mCurrentUser of
      Nothing -> throwError . ForbiddenError $ _ERROR_SERVICE_USER__MISSING_USER
      Just user ->
        unless
          (any (`elem` user.role.permissions) perms)
          (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission (need any): " ++ show perms))
  checkPermissionsAll perms = do
    mCurrentUser <- asks currentUser
    case mCurrentUser of
      Nothing -> throwError . ForbiddenError $ _ERROR_SERVICE_USER__MISSING_USER
      Just user ->
        unless
          (all (`elem` user.role.permissions) perms)
          (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permissions (need all): " ++ show perms))
  hasPermission perm = do
    mCurrentUser <- asks currentUser
    case mCurrentUser of
      Nothing -> return False
      Just user -> return $ perm `elem` user.role.permissions
