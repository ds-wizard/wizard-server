module Shared.Service.UserToken.Login.LoginService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U
import qualified Jose.Jwt as JWT

import Shared.Api.Resource.UserToken.LoginDTO
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.UserToken.Public
import Shared.Model.Cache.ServerCache
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.User
import Shared.Model.User.UserToken
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Service.Mail.Mailer
import Shared.Service.Tenant.Config.ConfigService
import qualified Shared.Service.User.WizardUserMapper as UserMapper
import Shared.Service.UserEmailLink.WizardUserEmailLinkService
import Shared.Service.UserToken.Login.LoginMapper
import Shared.Service.UserToken.Login.LoginValidation
import Shared.Service.UserToken.UserTokenMapper
import Shared.Service.UserToken.UserTokenUtil
import Shared.Util.Number
import Shared.Util.Token
import Shared.Util.Uuid

createLoginTokenFromCredentials :: WizardRequestContextC s m => LoginDTO -> Maybe String -> m UserTokenDTO
createLoginTokenFromCredentials reqDto mUserAgent =
  runInTransaction $ do
    mUser <- findUserByEmail' (fmap toLower reqDto.email)
    case mUser of
      Just user -> do
        validate reqDto user
        tcAuthentication <- getCurrentTenantConfigAuthentication
        validateLoginEnabled tcAuthentication user
        now <- liftIO getCurrentTime
        case (tcAuthentication.internal.twoFactorAuth.enabled, reqDto.code) of
          (False, _) -> do
            updateUserLastVisitedAtByUuid user.uuid now
            createLoginToken user mUserAgent Nothing
          (True, Nothing) -> do
            deleteUserEmailLinkByIdentity (U.toString user.uuid)
            let length = tcAuthentication.internal.twoFactorAuth.codeLength
            let min = 10 ^ (length - 1)
            let max = (10 ^ length) - 1
            code <- liftIO $ generateIntInRange min max
            createUserEmailLinkWithHash user.uuid TwoFactorAuthUserEmailLinkType user.tenantUuid (show code)
            sendTwoFactorAuthMail (UserMapper.toDTO user) (show code)
            return CodeRequiredDTO
          (True, Just code) -> do
            validateCode user code tcAuthentication
            deleteUserEmailLinkByIdentityAndHash (U.toString user.uuid) (show code)
            updateUserLastVisitedAtByUuid user.uuid now
            createLoginToken user mUserAgent Nothing
      Nothing -> throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

createLoginToken :: WizardRequestContextC s m => User -> Maybe String -> Maybe String -> m UserTokenDTO
createLoginToken user mUserAgent mSessionState =
  runInTransaction $ do
    serverConfig <- asks (.serverConfig')
    tcAuthentication <- getCurrentTenantConfigAuthentication
    let expiration = tcAuthentication.internal.sessionExpiration
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let claims = toUserTokenClaims user.uuid uuid user.tenantUuid now expiration
    (JWT.Jwt jwtToken) <- createSignedJwtToken claims
    let userToken = fromLoginDTO uuid user expiration serverConfig.general.secret mUserAgent mSessionState now (BS.unpack jwtToken)
    insertUserToken userToken
    return . toDTO $ userToken

deleteLoginTokenByValue :: WizardRequestContextC s m => Maybe String -> m ()
deleteLoginTokenByValue mTokenHeader = do
  case fmap separateToken mTokenHeader of
    Just (Just tokenValue) -> do
      userTokens <- findUserTokensByValue tokenValue
      traverse_ (\t -> deleteUserTokenByUuid t.uuid) userTokens
    _ -> return ()

deleteLoginTokenBySessionState :: WizardRequestContextC s m => Maybe String -> m ()
deleteLoginTokenBySessionState mSessionState = do
  case mSessionState of
    Just sessionState -> do
      userTokens <- findUserTokensBySessionState sessionState
      traverse_ (\t -> deleteUserTokenByUuid t.uuid) userTokens
    Nothing -> return ()
