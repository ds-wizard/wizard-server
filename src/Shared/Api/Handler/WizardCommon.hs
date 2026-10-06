module Shared.Api.Handler.WizardCommon where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (ask, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Handler.Common
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.UserToken.UserTokenClaimsDTO
import Shared.Constant.UserToken
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Cache.ServerCache
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Context.WizardServerContext
import Shared.Model.Error.Error
import Shared.Model.User.UserToken
import Shared.Service.User.UserService
import Shared.Service.UserToken.UserTokenValidation
import Shared.Util.Token

class (WizardServerContextC s sm, WizardRequestContextC r rm) => WizardHandlerC s sm r rm | sm -> rm where
  runIn :: Maybe String -> Maybe UserDTO -> Maybe U.UUID -> Maybe Bool -> TransactionState -> rm a -> sm a
  getScopedAuthServiceExecutor :: Maybe String -> Maybe String -> Maybe U.UUID -> Maybe Bool -> ((TransactionState -> rm a -> sm a) -> sm b) -> sm b

getAuthServiceExecutor :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> ((TransactionState -> rm a -> sm a) -> sm b) -> sm b
getAuthServiceExecutor mTokenHeader mServerUrl = getScopedAuthServiceExecutor mTokenHeader mServerUrl Nothing Nothing

runInUnauthService :: WizardHandlerC s sm r rm => Maybe String -> TransactionState -> rm a -> sm a
runInUnauthService mServerUrl = runIn mServerUrl Nothing Nothing Nothing

runInAuthService :: WizardHandlerC s sm r rm => Maybe String -> UserDTO -> TransactionState -> rm a -> sm a
runInAuthService mServerUrl user = runIn mServerUrl (Just user) Nothing Nothing

runInScopedAuthService :: WizardHandlerC s sm r rm => Maybe String -> UserDTO -> Maybe U.UUID -> Maybe Bool -> TransactionState -> rm a -> sm a
runInScopedAuthService mServerUrl user = runIn mServerUrl (Just user)

getMaybeAuthServiceExecutor :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> ((TransactionState -> rm a -> sm a) -> sm b) -> sm b
getMaybeAuthServiceExecutor mTokenHeader mServerUrl = getScopedMaybeAuthServiceExecutor mTokenHeader mServerUrl Nothing Nothing

getScopedMaybeAuthServiceExecutor :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Maybe U.UUID -> Maybe Bool -> ((TransactionState -> rm a -> sm a) -> sm b) -> sm b
getScopedMaybeAuthServiceExecutor (Just tokenHeader) mServerUrl mW mTenant callback = getScopedAuthServiceExecutor (Just tokenHeader) mServerUrl mW mTenant callback
getScopedMaybeAuthServiceExecutor Nothing mServerUrl mW mTenant callback = callback (runIn mServerUrl Nothing mW mTenant)

validateJwtToken :: WizardServerContextC s sm => String -> sm UserTokenClaimsDTO
validateJwtToken tokenHeader = do
  serverContext <- ask
  now <- liftIO getCurrentTime
  case separateToken tokenHeader of
    Just jwtToken -> do
      eUserTokenClaims <- decodeAndValidateJwtToken jwtToken serverContext.serverConfig'.general.rsaPrivateKey userTokenVersion now
      case eUserTokenClaims of
        Right userTokenClaims -> return userTokenClaims
        Left error -> throwError =<< sendError (UnauthorizedError error)
    Nothing -> throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

isTokenExistsInDb :: WizardHandlerC s sm r rm => UserTokenClaimsDTO -> Maybe String -> sm UserToken
isTokenExistsInDb userTokenClaims mServerUrl = do
  let tokenUuid = userTokenClaims.tokenUuid
  runInUnauthService mServerUrl NoTransaction $ catchError (findUserTokenByUuid tokenUuid) (handleError tokenUuid)
  where
    handleError tokenUuid (NotExistsError _) = throwError $ UnauthorizedError (_ERROR_VALIDATION__TOKEN_ABSENCE . U.toString $ tokenUuid)
    handleError tokenUuid error = throwError error

getCurrentUser :: WizardHandlerC s sm r rm => UserTokenClaimsDTO -> Maybe String -> sm UserDTO
getCurrentUser userTokenClaims mServerUrl = do
  let userUuid = userTokenClaims.userUuid
  runInUnauthService mServerUrl NoTransaction $ catchError (getUserById userUuid) (handleError userUuid)
  where
    handleError userUuid (NotExistsError _) = throwError $ UnauthorizedError (_ERROR_VALIDATION__USER_ABSENCE . U.toString $ userUuid)
    handleError userUuid error = throwError error
