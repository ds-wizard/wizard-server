module Shared.Service.Context.ContextService where

import qualified Control.Exception.Base as E
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson (Value (..))
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as U

import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Model.Context.AclContext
import Shared.Model.Context.ContextResult
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Sentry.SentryEvent
import Shared.Model.Tenant.Tenant
import Shared.Service.Sentry.SentryService
import qualified Shared.Service.User.WizardUserMapper as UM
import Shared.Util.Logger
import Shared.Util.Sentry
import Shared.Util.Uuid

runFunctionForAllTenants :: WizardRequestContextC s m => String -> m (ContextResult, Maybe String) -> m ()
runFunctionForAllTenants functionName function = do
  tenants <- findTenants
  traverse_ (\tenant -> runFunctionUnderDifferentUserAndTenant Nothing tenant.uuid functionName function) tenants

runFunctionUnderDifferentUserAndTenant
  :: WizardRequestContextC s m => Maybe User -> U.UUID -> String -> m (ContextResult, Maybe String) -> m ()
runFunctionUnderDifferentUserAndTenant mUser tenantUuid functionName function = do
  logInfoI
    _CMP_SERVICE
    ( f'
        "Running '%s' with tenant ('%s') and user ('%s') started"
        [functionName, U.toString tenantUuid, show . fmap (U.toString . ((.uuid))) $ mUser]
    )
  context <- ask
  newTraceUuid <- liftIO generateUuid
  eResult <- liftIO . E.try $ runRequestContextWithRequestContext function (updateContext mUser tenantUuid newTraceUuid context)
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
updateContext :: WizardRequestContextType s => Maybe User -> U.UUID -> U.UUID -> s -> s
updateContext mUser aUuid newTraceUuid =
  setTraceUuid newTraceUuid . setCurrentUser (fmap UM.toDTO mUser) . setTenantUuid aUuid

sendToSentry :: WizardRequestContextC s m => Maybe User -> U.UUID -> String -> Maybe String -> m ()
sendToSentry mUser tenantUuid functionName mErrorMessage =
  captureAppSentryEvent
    (toSentryEvent "contextLogger" (normalizeMessage . fromMaybe "" $ mErrorMessage))
      { culprit = Just functionName
      , fingerprint = [functionName]
      , tags = [("function", functionName), ("tenantUuid", U.toString tenantUuid)]
      , extra = [("lastErrorMessage", String . T.pack . fromMaybe "" $ mErrorMessage)]
      , interfaces = toUserInterface (fmap (U.toString . (.uuid)) mUser) Nothing
      }
