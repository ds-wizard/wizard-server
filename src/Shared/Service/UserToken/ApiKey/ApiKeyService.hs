module Shared.Service.UserToken.ApiKey.ApiKeyService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.Time
import qualified Jose.Jwt as JWT

import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.UserToken.ApiKeyCreateDTO
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Cache.ServerCache
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.User
import Shared.Model.User.UserToken
import Shared.Service.Mail.Mailer
import Shared.Service.UserToken.ApiKey.ApiKeyMapper
import Shared.Service.UserToken.UserTokenMapper
import Shared.Service.UserToken.UserTokenUtil
import Shared.Util.Uuid

createApiKey :: WizardRequestContextC s m => ApiKeyCreateDTO -> Maybe String -> m UserTokenDTO
createApiKey reqDto mUserAgent =
  runInTransaction $ do
    serverConfig <- asks (.serverConfig')
    uuid <- liftIO generateUuid
    userDto <- getCurrentUser
    user <- findUserByUuid userDto.uuid
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let claims = toUserTokenClaimsWithExpiration user.uuid uuid user.tenantUuid now reqDto.expiresAt
    (JWT.Jwt jwtToken) <- createSignedJwtToken claims
    let userToken = fromApiKeyDTO reqDto uuid user.uuid serverConfig.general.secret mUserAgent tenantUuid now (BS.unpack jwtToken)
    insertUserToken userToken
    sendApiKeyCreatedMail userDto userToken
    return . toDTO $ userToken

expireApiKeys :: WizardRequestContextC s m => m ()
expireApiKeys = do
  userTokens <- findApiUserTokensWithCloseExpiration
  traverse_
    ( \userToken -> do
        user <- findUserByUuidAndTenantUuidSystem userToken.userUuid userToken.tenantUuid
        sendApiKeyExpirationMail user userToken
    )
    userTokens
