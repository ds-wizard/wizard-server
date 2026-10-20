module Shared.Service.Mcp.Tool.McpWaitToolCommon where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader (ask, asks, liftIO)
import Data.Time

import Shared.Constant.Mcp
import Shared.Model.Context.ProgressReporter
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Service.Mcp.Server.McpToolService (rethrowContextResult, runWithToolConnection)
import Shared.Util.Error (tryError)

withShortConnection :: WizardRequestContextC s m => m a -> m a
withShortConnection action = do
  context <- ask
  rethrowContextResult =<< liftIO (runWithToolConnection Transactional context (runRequestContextWithRequestContext (tryError action)))

pollWhile :: WizardRequestContextC s m => (a -> Maybe String) -> Int -> m a -> a -> m a
pollWhile pendingMessage waitSeconds refresh current = do
  start <- liftIO getCurrentTime
  pollFrom pendingMessage start (fromIntegral waitSeconds) refresh current

pollFrom :: WizardRequestContextC s m => (a -> Maybe String) -> UTCTime -> Double -> m a -> a -> m a
pollFrom pendingMessage start total refresh current = do
  elapsed <- realToFrac . (`diffUTCTime` start) <$> liftIO getCurrentTime
  case pendingMessage current of
    Just message -> do
      reportProgress (min elapsed total) (Just total) message
      if elapsed < total
        then do
          liftIO (threadDelay mcpPollInterval)
          pollFrom pendingMessage start total refresh =<< refresh
        else return current
    Nothing -> return current

reportProgress :: WizardRequestContextC s m => Double -> Maybe Double -> String -> m ()
reportProgress progress total message = do
  ProgressReporter report <- asks (.progressReporter')
  liftIO (report progress total message)
