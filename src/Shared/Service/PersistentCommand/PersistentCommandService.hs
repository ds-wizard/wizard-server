module Shared.Service.PersistentCommand.PersistentCommandService where

import qualified Control.Exception.Base as E
import Control.Monad (forever, unless, void, when)
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson (Value (..))
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.DAO.Common (runInTransaction)
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.RequestContext
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.PersistentCommandSimple
import Shared.Model.Sentry.SentryEvent
import Shared.Model.User.RolePermission
import Shared.Service.Acl.AclService
import Shared.Service.Sentry.SentryService
import Shared.Util.Logger
import Shared.Util.Sentry

runPersistentCommands
  :: (Show identity, FromField identity, ToField identity, RequestContextC s sc m)
  => (function -> requestContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommandSimple identity -> s -> m requestContext)
  -> (PersistentCommand identity -> function)
  -> [String]
  -> m ()
runPersistentCommands runRequestContextWithRequestContext' updateContext execute components = do
  checkPermission _DEV_USE_ROLE_PERMISSION
  commands <- findPersistentCommandsForRetryByStates components
  unless
    (null commands)
    ( do
        traverse_ (runPersistentCommand runRequestContextWithRequestContext' updateContext execute False) commands
        runPersistentCommands runRequestContextWithRequestContext' updateContext execute components
    )

runPersistentCommand
  :: (Show identity, FromField identity, ToField identity, RequestContextC s sc m)
  => (function -> requestContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommandSimple identity -> s -> m requestContext)
  -> (PersistentCommand identity -> function)
  -> Bool
  -> PersistentCommandSimple identity
  -> m ()
runPersistentCommand runRequestContextWithRequestContext' updateContext execute force commandSimple = do
  context <- ask
  updatedContext <- updateContext commandSimple context
  executePersistentCommandByUuid runRequestContextWithRequestContext' execute force commandSimple.uuid updatedContext

executePersistentCommandByUuid
  :: (Show identity, FromField identity, ToField identity, RequestContextC s sc m)
  => (function -> requestContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommand identity -> function)
  -> Bool
  -> U.UUID
  -> requestContext
  -> m ()
executePersistentCommandByUuid runRequestContextWithRequestContext' execute force uuid context = do
  logInfoI _CMP_SERVICE (f' "Running command '%s'" [U.toString uuid])
  runInTransaction logInfoI logWarnI $ do
    mCommand <- lockPersistentCommandByUuid uuid
    case mCommand of
      Nothing -> logInfoI _CMP_SERVICE (f' "Command '%s' is already running somewhere else" [U.toString uuid])
      Just command ->
        when
          (command.state == NewPersistentCommandState || (command.state == ErrorPersistentCommandState && command.attempts < command.maxAttempts) || force)
          ( do
              eResult <- liftIO . E.try $ runRequestContextWithRequestContext' (execute command) context
              let (resultState, mErrorMessage) =
                    case eResult :: Either E.SomeException (Either String (PersistentCommandState, Maybe String)) of
                      Right (Right (resultState, mErrorMessage)) -> (resultState, mErrorMessage)
                      Right (Left error) -> (ErrorPersistentCommandState, Just error)
                      Left exception -> (ErrorPersistentCommandState, Just . show $ exception)
              context <- ask
              now <- liftIO getCurrentTime
              let updatedCommand =
                    command
                      { state = resultState
                      , lastTraceUuid = Just context.traceUuid'
                      , lastErrorMessage = mErrorMessage
                      , attempts = command.attempts + 1
                      , updatedAt = now
                      }
              when (resultState == ErrorPersistentCommandState) (sendToSentry updatedCommand)
              updatePersistentCommandByUuid updatedCommand
              logInfoI _CMP_SERVICE (f' "Command finished with following state: '%s'" [show resultState])
          )

runPersistentCommandChannelListener
  :: (Show identity, FromField identity, ToField identity, RequestContextC s sc m)
  => (function -> requestContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommandSimple identity -> s -> m requestContext)
  -> (PersistentCommand identity -> function)
  -> [String]
  -> m ()
runPersistentCommandChannelListener runRequestContextWithRequestContext updateContext execute components = do
  listenPersistentCommandChannel
  forever $ do
    _ <- getChannelNotification
    runPersistentCommands runRequestContextWithRequestContext updateContext execute components

retryPersistentCommandsForLambda :: RequestContextC s sc m => [String] -> m ()
retryPersistentCommandsForLambda ownComponents = do
  context <- ask
  let components = filter (`notElem` ownComponents) . fmap (\lf -> lf.component) $ context.serverConfig'.persistentCommand'.lambdaFunctions
  persistentCommands <- findPersistentCommandsForRetryByStates components
  traverse_ retryPersistentCommandForLambda (persistentCommands :: [PersistentCommandSimple U.UUID])

retryPersistentCommandForLambda :: (Show identity, FromField identity, ToField identity, RequestContextC s sc m) => PersistentCommandSimple identity -> m ()
retryPersistentCommandForLambda command = do
  context <- ask
  case L.find (\lf -> lf.component == command.component) context.serverConfig'.persistentCommand'.lambdaFunctions of
    Just lf -> void $ invokeLambdaFunction lf
    Nothing -> logWarnI _CMP_DATABASE (f' "No lambda function found for persistent command '%s'" [U.toString command.uuid])

-- --------------------------------
-- PRIVATE
-- --------------------------------

sendToSentry :: (Show identity, FromField identity, ToField identity, RequestContextC s sc m) => PersistentCommand identity -> m ()
sendToSentry command =
  captureAppSentryEvent
    (toSentryEvent "persistentCommandLogger" (normalizeMessage . fromMaybe "" $ command.lastErrorMessage))
      { culprit = Just (command.component ++ "/" ++ command.function)
      , fingerprint = [command.component, command.function]
      , tags =
          [ ("component", command.component)
          , ("function", command.function)
          , ("tenantUuid", U.toString command.tenantUuid)
          ]
      , extra =
          [ ("uuid", String . T.pack . U.toString $ command.uuid)
          , ("attempts", String . T.pack . show $ command.attempts)
          , ("maxAttempts", String . T.pack . show $ command.maxAttempts)
          , ("body", String . T.pack $ command.body)
          , ("lastErrorMessage", String . T.pack . fromMaybe "" $ command.lastErrorMessage)
          ]
      , interfaces = toUserInterface (fmap show command.createdBy) Nothing
      }
