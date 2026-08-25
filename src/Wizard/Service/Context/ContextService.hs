module Wizard.Service.Context.ContextService where

import qualified Control.Exception.Base as E
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson (Value (..))
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as U

import Shared.Common.Model.Context.ContextResult
import Shared.Common.Model.Sentry.SentryEvent
import Shared.Common.Service.Sentry.SentryService
import Shared.Common.Util.Logger
import Shared.Common.Util.Sentry
import Shared.Common.Util.Uuid
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextMappers
import Wizard.Model.Tenant.Tenant
import qualified Wizard.Service.User.UserMapper as UM

runFunctionForAllTenants :: String -> AppContextM (ContextResult, Maybe String) -> AppContextM ()
runFunctionForAllTenants functionName function = do
  tenants <- findTenants
  traverse_ (\tenant -> runFunctionUnderDifferentUserAndTenant Nothing tenant.uuid functionName function) tenants

runFunctionUnderDifferentUserAndTenant
  :: Maybe User -> U.UUID -> String -> AppContextM (ContextResult, Maybe String) -> AppContextM ()
runFunctionUnderDifferentUserAndTenant mUser tenantUuid functionName function = do
  logInfoI
    _CMP_SERVICE
    ( f'
        "Running '%s' with tenant ('%s') and user ('%s') started"
        [functionName, U.toString tenantUuid, show . fmap (U.toString . ((.uuid))) $ mUser]
    )
  context <- ask
  newTraceUuid <- liftIO generateUuid
  eResult <- liftIO . E.try $ runAppContextWithAppContext function (updateContext mUser tenantUuid newTraceUuid context)
  let (resultState, mReturnedMessage) =
        case eResult :: Either E.SomeException (Either String (ContextResult, Maybe String)) of
          Right (Right (resultState, mErrorMessage)) -> (resultState, mErrorMessage)
          Right (Left error) -> (ErrorContextResult, Just error)
          Left exception -> (ErrorContextResult, Just . show $ exception)
  case resultState of
    SuccessContextResult ->
      logInfoI
        _CMP_SERVICE
        ( f'
            "Running '%s' with tenant ('%s') and user ('%s') finished successfully. It returns: '%s''"
            [functionName, U.toString tenantUuid, show . fmap (U.toString . ((.uuid))) $ mUser, show mReturnedMessage]
        )
    ErrorContextResult -> do
      logInfoI
        _CMP_SERVICE
        ( f'
            "Running '%s' with tenant ('%s') and user ('%s') failed. The reason is: '%s''"
            [functionName, U.toString tenantUuid, show . fmap (U.toString . ((.uuid))) $ mUser, show mReturnedMessage]
        )
      sendToSentry mUser tenantUuid functionName mReturnedMessage

-- --------------------------------
-- PRIVATE
-- --------------------------------
updateContext :: Maybe User -> U.UUID -> U.UUID -> AppContext -> AppContext
updateContext mUser aUuid newTraceUuid context =
  context
    { currentTenantUuid = aUuid
    , currentUser = fmap UM.toDTO mUser
    , traceUuid = newTraceUuid
    }

sendToSentry :: Maybe User -> U.UUID -> String -> Maybe String -> AppContextM ()
sendToSentry mUser tenantUuid functionName mErrorMessage =
  captureAppSentryEvent
    (toSentryEvent "contextLogger" (normalizeMessage . fromMaybe "" $ mErrorMessage))
      { culprit = Just functionName
      , fingerprint = [functionName]
      , tags = [("function", functionName), ("tenantUuid", U.toString tenantUuid)]
      , extra = [("lastErrorMessage", String . T.pack . fromMaybe "" $ mErrorMessage)]
      , interfaces = toUserInterface (fmap (U.toString . (.uuid)) mUser) Nothing
      }
