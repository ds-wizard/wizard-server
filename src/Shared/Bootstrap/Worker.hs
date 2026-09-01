module Shared.Bootstrap.Worker (
  worker,
) where

import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import System.Cron
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigINT, sigTERM)
import Prelude hiding (log)

import Shared.Model.Config.ServerConfig
import Shared.Model.Context.RequestContext
import Shared.Model.Context.ServerContext
import Shared.Model.Worker.CronWorker
import Shared.Util.Logger

worker
  :: ( RequestContextC s sc requestContextM
     , ServerContextType serverContext sc
     )
  => (requestContextM () -> serverContext -> IO (Either String ()))
  -> (requestContextM () -> serverContext -> IO (Either String ()))
  -> MVar ()
  -> serverContext
  -> [CronWorker serverContext requestContextM]
  -> (serverContext -> LoggingT IO [ThreadId])
  -> IO ()
worker runRequestContextWithServerContext runRequestContextWithServerContext'' shutdownFlag context cronWorkers permanentWorker =
  let loggingLevel = context.serverConfig'.logging'.level
   in runLogging loggingLevel $ do
        cronWorkerThreadIds <- cronJob runRequestContextWithServerContext runRequestContextWithServerContext'' context cronWorkers
        permanentWorkerThreadIds <- permanentWorker context
        setupHandlers loggingLevel shutdownFlag (cronWorkerThreadIds ++ permanentWorkerThreadIds)

-- ------------------------------------------------------------------
cronJob
  :: ( RequestContextC s sc requestContextM
     , ServerContextType serverContext sc
     )
  => (requestContextM () -> serverContext -> IO (Either String ()))
  -> (requestContextM () -> serverContext -> IO (Either String ()))
  -> serverContext
  -> [CronWorker serverContext requestContextM]
  -> LoggingT IO [ThreadId]
cronJob runRequestContextWithServerContext runRequestContextWithServerContext'' context workers = do
  logInfo _CMP_WORKER "scheduling workers started"
  threadIds <- liftIO . execSchedule $ traverse_ (workerFn runRequestContextWithServerContext runRequestContextWithServerContext'' context) workers
  logInfo _CMP_WORKER "scheduling workers completed"
  return threadIds

setupHandlers loggingLevel shutdownFlag threadIds = do
  logInfo _CMP_WORKER "installing handlers"
  liftIO $ installHandler sigINT (handler loggingLevel shutdownFlag threadIds "sigINT") Nothing
  liftIO $ installHandler sigTERM (handler loggingLevel shutdownFlag threadIds "sigTERM") Nothing
  logInfo _CMP_WORKER "handlers installed"

handler loggingLevel shutdownFlag threadIds typeSignal =
  CatchOnce . runLogging loggingLevel $ do
    logInfo _CMP_WORKER "shutting down workers: started"
    liftIO $ traverse killThread threadIds
    logInfo _CMP_WORKER "shutting down workers: notifying web server"
    liftIO $ putMVar shutdownFlag ()
    logInfo _CMP_WORKER "shutting down workers: completed"

workerFn
  :: ( MonadSchedule m
     , Applicative m
     , RequestContextC s sc requestContextM
     , ServerContextType serverContext sc
     )
  => (requestContextM () -> serverContext -> IO (Either String ()))
  -> (requestContextM () -> serverContext -> IO (Either String ()))
  -> serverContext
  -> CronWorker serverContext requestContextM
  -> m ()
workerFn runRequestContextWithServerContext runRequestContextWithServerContext'' context cronWorker =
  when
    (cronWorker.condition context)
    (addJob (job runRequestContextWithServerContext runRequestContextWithServerContext'' cronWorker context) (T.pack . cronWorker.cron $ context))

job
  :: ( RequestContextC requestContext sc requestContextM
     , ServerContextType serverContext sc
     )
  => (requestContextM () -> serverContext -> IO (Either String ()))
  -> (requestContextM () -> serverContext -> IO (Either String ()))
  -> CronWorker serverContext requestContextM
  -> serverContext
  -> IO ()
job runRequestContextWithServerContext runRequestContextWithServerContext'' cronWorker context =
  let loggingLevel = context.serverConfig'.logging'.level
   in runLogging loggingLevel $ do
        logInfo _CMP_WORKER . f' "%s: starting" $ [cronWorker.name]
        if cronWorker.wrapInTransaction
          then liftIO $ runRequestContextWithServerContext cronWorker.function context
          else liftIO $ runRequestContextWithServerContext'' cronWorker.function context
        logInfo _CMP_WORKER . f' "%s: ended" $ [cronWorker.name]
