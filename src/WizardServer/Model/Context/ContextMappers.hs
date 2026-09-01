module WizardServer.Model.Context.ContextMappers where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.IORef (newIORef)
import Data.Pool
import Data.Time
import qualified Data.UUID as U
import Shared.Constant.Tenant
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Service.User.WizardUserMapper
import Shared.Util.Logger
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext
import WizardServer.Model.Context.ServerContext

runRequestContextWithServerContext :: RequestContextM a -> ServerContext -> IO (Either String a)
runRequestContextWithServerContext function serverContext =
  requestContextFromServerContext defaultTenantUuid (Just . toDTO $ userSystem) Transactional serverContext $
    runRequestContextWithRequestContext function

runRequestContextWithServerContext' :: RequestContextM a -> ServerContext -> U.UUID -> IO (Either String a)
runRequestContextWithServerContext' function serverContext tenantUuid =
  requestContextFromServerContext tenantUuid (Just . toDTO $ userSystem) Transactional serverContext $
    runRequestContextWithRequestContext function

runRequestContextWithServerContext'' :: RequestContextM a -> ServerContext -> IO (Either String a)
runRequestContextWithServerContext'' function serverContext =
  requestContextFromServerContext defaultTenantUuid (Just . toDTO $ userSystem) NoTransaction serverContext $
    runRequestContextWithRequestContext function

runMonads fn context = runExceptT $ runLogging' context $ runReaderT fn context

runLogging' context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel

requestContextFromServerContext tenantUuid mUser transactionState serverContext callback = do
  cTraceUuid <- generateUuid
  cBreadcrumbs <- liftIO (newIORef [])
  now <- liftIO getCurrentTime
  let requestContext =
        RequestContext
          { serverConfig = serverContext.serverConfig
          , buildInfoConfig = serverContext.buildInfoConfig
          , dbPool = serverContext.dbPool
          , dbConnection = Nothing
          , s3Client = serverContext.s3Client
          , httpClientManager = serverContext.httpClientManager
          , restrictedHttpClientManager = serverContext.restrictedHttpClientManager
          , registryClient = serverContext.registryClient
          , traceUuid = cTraceUuid
          , breadcrumbs = cBreadcrumbs
          , currentTenantUuid = tenantUuid
          , currentUser = mUser
          , shutdownFlag = serverContext.shutdownFlag
          , cache = serverContext.cache
          }
  case transactionState of
    Transactional -> do
      withResource serverContext.dbPool $ \dbConn -> do
        let requestContextWithConn = requestContext {dbConnection = Just dbConn}
        callback requestContextWithConn
    NoTransaction -> do
      callback requestContext

serverContextFromRequestContext :: RequestContext -> ServerContext
serverContextFromRequestContext requestContext =
  ServerContext
    { serverConfig = requestContext.serverConfig
    , buildInfoConfig = requestContext.buildInfoConfig
    , dbPool = requestContext.dbPool
    , s3Client = requestContext.s3Client
    , httpClientManager = requestContext.httpClientManager
    , restrictedHttpClientManager = requestContext.restrictedHttpClientManager
    , registryClient = requestContext.registryClient
    , shutdownFlag = requestContext.shutdownFlag
    , cache = requestContext.cache
    }
