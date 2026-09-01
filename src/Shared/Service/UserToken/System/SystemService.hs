module Shared.Service.UserToken.System.SystemService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Jose.Jwk as JWK
import qualified Jose.Jwt as JWT

import Shared.Api.Resource.UserToken.UserTokenClaimsDTO
import Shared.Api.Resource.UserToken.UserTokenClaimsJM ()
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Constant.UserToken
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.Admin.Runner
import Shared.Model.Cache.ServerCache
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.User
import Shared.Service.Tenant.Config.ConfigService
import Shared.Service.UserToken.System.SystemMapper
import Shared.Service.UserToken.System.SystemValidation
import Shared.Service.UserToken.UserTokenMapper
import Shared.Service.UserToken.UserTokenUtil
import Shared.Util.Uuid

createSystemToken :: WizardRequestContextC s m => String -> Maybe String -> m UserTokenDTO
createSystemToken token mUserAgent =
  runInTransaction $ do
    now <- liftIO getCurrentTime
    (JWK.JwkSet keys) <- retrieveJwtPublicKeys
    eUserTokenClaims <- decodeAndValidateJwtToken token keys userTokenVersion now
    case eUserTokenClaims of
      Right userTokenClaims -> do
        serverConfig <- asks (.serverConfig')
        user <- findUserByUuidAndTenantUuidSystem userTokenClaims.userUuid userTokenClaims.tenantUuid
        tcAuthentication <- getTenantConfigAuthenticationByUuid user.tenantUuid
        let expiration = tcAuthentication.internal.sessionExpiration
        uuid <- liftIO generateUuid
        updateUserLastVisitedAtByUuid user.uuid now
        let claims = toUserTokenClaims user.uuid uuid user.tenantUuid now expiration
        (JWT.Jwt jwtToken) <- createSignedJwtToken claims
        let userToken = fromSystemDTO uuid user expiration serverConfig.general.secret mUserAgent Nothing now (BS.unpack jwtToken)
        insertUserToken userToken
        return . toDTO $ userToken
      Left error -> throwError . UnauthorizedError $ error
