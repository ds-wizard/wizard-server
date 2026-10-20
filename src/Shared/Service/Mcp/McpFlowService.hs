module Shared.Service.Mcp.McpFlowService where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.Reader (asks, lift, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U
import qualified Jose.Jwk as JWK
import qualified Jose.Jwt as JWT

import Shared.Api.Resource.Mcp.Client.McpClientDTO
import Shared.Api.Resource.Mcp.Code.McpCodeCreateDTO
import Shared.Api.Resource.Mcp.Code.McpCodeDTO
import Shared.Api.Resource.Mcp.Metadata.McpMetadataDTO
import Shared.Api.Resource.Mcp.Metadata.McpProtectedResourceDTO
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateDTO
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationDTO
import Shared.Api.Resource.Mcp.Token.McpTokenCreateDTO
import Shared.Api.Resource.Mcp.Token.McpTokenDTO
import Shared.Api.Resource.User.UserDTO (UserDTO (..))
import Shared.Database.DAO.Mcp.McpAuthorizationCodeDAO
import Shared.Database.DAO.Mcp.McpClientDAO
import Shared.Database.DAO.Mcp.McpRefreshTokenDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Cache.ServerCache
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpAuthorizationCode
import Shared.Model.Mcp.McpClient
import Shared.Model.Mcp.McpOAuthError
import Shared.Model.Mcp.McpRefreshToken
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.User
import Shared.Model.User.UserToken
import Shared.Service.Mcp.McpAuditLog
import Shared.Service.Mcp.McpFlowMapper
import Shared.Service.Mcp.McpFlowValidation
import Shared.Service.Tenant.TenantHelper
import Shared.Service.UserToken.UserTokenMapper
import Shared.Service.UserToken.UserTokenUtil
import Shared.Util.Crypto (generateRandomString, hashSHA256)
import Shared.Util.Date
import Shared.Util.Jwk
import Shared.Util.Uuid

type McpOAuthM m = ExceptT McpOAuthError m

mcpAccessTokenExpirationHours :: Integer
mcpAccessTokenExpirationHours = 1

mcpGrantExpiration :: NominalDiffTime
mcpGrantExpiration = 90 * nominalDay

getMcpProtectedResource :: WizardRequestContextC s m => m McpProtectedResourceDTO
getMcpProtectedResource = toMcpProtectedResourceDTO <$> getCurrentTenant

getMcpMetadata :: WizardRequestContextC s m => m McpMetadataDTO
getMcpMetadata = toMcpMetadataDTO <$> getCurrentTenant

getMcpKeys :: WizardRequestContextC s m => m JWK.JwkSet
getMcpKeys = do
  serverConfig <- asks (.serverConfig')
  return . toJwkSet $ serverConfig.general.rsaPrivateKey

getMcpClient :: WizardRequestContextC s m => String -> m (Either McpOAuthError McpClientDTO)
getMcpClient clientId = do
  mClient <- findMcpClientByClientId' clientId
  return $ maybe (Left $ unknownClient clientId) (Right . toMcpClientDTO) mClient

registerMcpClient :: WizardRequestContextC s m => McpClientRegistrationCreateDTO -> m (Either McpOAuthError McpClientRegistrationDTO)
registerMcpClient reqDto =
  runInTransaction . runExceptT $ do
    authMethod <- liftEither $ validateTokenEndpointAuthMethod reqDto.tokenEndpointAuthMethod
    liftEither $ validateRedirectUris reqDto.redirectUris
    liftEither $ validateClientName reqDto.clientName
    tenantUuid <- asks (.tenantUuid')
    uuid <- liftIO generateUuid
    clientId <- liftIO $ generateRandomString 32
    clientSecret <- liftIO $ if authMethod == "none" then return "" else generateRandomString 64
    now <- liftIO getCurrentTime
    let client = toMcpClient uuid clientId clientSecret authMethod reqDto tenantUuid now
    lift $ insertMcpClient client
    return $ toMcpClientRegistrationDTO client

createMcpCodeForCurrentUser :: WizardRequestContextC s m => McpCodeCreateDTO -> m McpCodeDTO
createMcpCodeForCurrentUser reqDto =
  runInTransaction $ do
    userDto <- getCurrentUser
    user <- findUserByUuid userDto.uuid
    validateMcpPermission user
    client <- findMcpClientByClientId reqDto.clientId
    validateRedirectUri client reqDto.redirectUri
    validateCodeChallenge reqDto.codeChallenge reqDto.codeChallengeMethod
    uuid <- liftIO generateUuid
    code <- liftIO $ generateRandomString 256
    now <- liftIO getCurrentTime
    insertMcpAuthorizationCode $ toMcpAuthorizationCode uuid (hashSHA256 code) client user reqDto now
    return $ McpCodeDTO {code = code}

exchangeMcpToken :: WizardRequestContextC s m => Maybe String -> McpTokenCreateDTO -> m (Either McpOAuthError McpTokenDTO)
exchangeMcpToken mAuthHeader reqDto =
  runInTransaction . runExceptT $ do
    client <- authenticateMcpClient mAuthHeader reqDto
    grantType <- orOAuthError (invalidRequest "Missing grant_type") reqDto.grantType
    case grantType of
      "authorization_code" -> exchangeMcpAuthorizationCode client reqDto
      "refresh_token" -> refreshMcpGrant client reqDto
      grantType -> throwError $ unsupportedGrantType grantType

authenticateMcpClient :: WizardRequestContextC s m => Maybe String -> McpTokenCreateDTO -> McpOAuthM m McpClient
authenticateMcpClient mAuthHeader reqDto = do
  (clientId, mClientSecret) <- liftEither $ resolveClientCredentials mAuthHeader reqDto
  client <- lift (findMcpClientByClientId' clientId) >>= orOAuthError (invalidClient "Invalid client credentials")
  liftEither $ validateClientSecret client mClientSecret
  return client

exchangeMcpAuthorizationCode :: WizardRequestContextC s m => McpClient -> McpTokenCreateDTO -> McpOAuthM m McpTokenDTO
exchangeMcpAuthorizationCode client reqDto = do
  codeHash <- hashSHA256 <$> orOAuthError (invalidRequest "Missing code") reqDto.code
  authCode <- lift (findMcpAuthorizationCodeByCode' codeHash) >>= orOAuthError (invalidGrant "Invalid authorization code")
  now <- liftIO getCurrentTime
  liftEither $ validateAuthorizationCode authCode client reqDto.redirectUri now
  liftEither $ validateCodeVerifier authCode client reqDto.codeVerifier
  user <- lift $ findUserByUuid authCode.userUuid
  liftEither $ validateMcpGrantPermission user
  deleted <- lift $ deleteMcpAuthorizationCodeByCode codeHash
  when (deleted /= 1) . throwError $ invalidGrant "Invalid authorization code"
  lift $ issueMcpGrant user client authCode.toolsets now

refreshMcpGrant :: WizardRequestContextC s m => McpClient -> McpTokenCreateDTO -> McpOAuthM m McpTokenDTO
refreshMcpGrant client reqDto = do
  tokenHash <- hashSHA256 <$> orOAuthError (invalidRequest "Missing refresh_token") reqDto.refreshToken
  refreshToken <- lift (findMcpRefreshTokenByTokenHash' tokenHash) >>= maybe (revokeReusedMcpGrant tokenHash) return
  now <- liftIO getCurrentTime
  liftEither $ validateRefreshToken refreshToken client now
  userToken <- lift $ findUserTokenByUuid refreshToken.userTokenUuid
  user <- lift $ findUserByUuid userToken.userUuid
  liftEither $ validateMcpGrantPermission user
  deleted <- lift $ deleteMcpRefreshTokenByUuid refreshToken.uuid
  when (deleted /= 1) $ revokeReusedMcpGrant tokenHash
  lift $ rotateMcpGrant refreshToken userToken user client now

revokeReusedMcpGrant :: WizardRequestContextC s m => String -> McpOAuthM m a
revokeReusedMcpGrant tokenHash = do
  mRotated <- lift $ findMcpRefreshTokenByPreviousTokenHash' tokenHash
  traverse_ (lift . deleteUserTokenByUuid . (.userTokenUuid)) mRotated
  throwError $ invalidGrant "Invalid refresh token"

issueMcpGrant :: WizardRequestContextC s m => User -> McpClient -> [McpToolset] -> UTCTime -> m McpTokenDTO
issueMcpGrant user client toolsets now = do
  serverConfig <- asks (.serverConfig')
  uuid <- liftIO generateUuid
  accessToken <- createMcpAccessToken user uuid now
  let userToken = toMcpUserToken uuid user client serverConfig.general.secret now (addUTCTime mcpGrantExpiration now) accessToken
  insertUserToken userToken
  refreshToken <- createMcpRefreshToken userToken client toolsets Nothing now
  al_connectMcp user client uuid
  return $ toMcpTokenDTO accessToken mcpAccessTokenExpirationSeconds refreshToken

rotateMcpGrant :: WizardRequestContextC s m => McpRefreshToken -> UserToken -> User -> McpClient -> UTCTime -> m McpTokenDTO
rotateMcpGrant previous userToken user client now = do
  accessToken <- createMcpAccessToken user userToken.uuid now
  let renewed = renewMcpUserToken userToken accessToken (addUTCTime mcpGrantExpiration now)
  updateUserTokenByUuid renewed
  refreshToken <- createMcpRefreshToken renewed client previous.toolsets (Just previous.tokenHash) now
  return $ toMcpTokenDTO accessToken mcpAccessTokenExpirationSeconds refreshToken

createMcpAccessToken :: WizardRequestContextC s m => User -> U.UUID -> UTCTime -> m String
createMcpAccessToken user tokenUuid now = do
  let claims = toUserTokenClaims user.uuid tokenUuid user.tenantUuid now mcpAccessTokenExpirationHours
  (JWT.Jwt jwtToken) <- createSignedJwtToken claims
  return $ BS.unpack jwtToken

createMcpRefreshToken :: WizardRequestContextC s m => UserToken -> McpClient -> [McpToolset] -> Maybe String -> UTCTime -> m String
createMcpRefreshToken userToken client toolsets mPreviousTokenHash now = do
  uuid <- liftIO generateUuid
  refreshToken <- liftIO $ generateRandomString 192
  insertMcpRefreshToken $ toMcpRefreshToken uuid (hashSHA256 refreshToken) mPreviousTokenHash userToken client toolsets now
  return refreshToken

deleteMcpTokenForCurrentUser :: WizardRequestContextC s m => U.UUID -> m ()
deleteMcpTokenForCurrentUser uuid =
  runInTransaction $ do
    currentUser <- getCurrentUser
    userToken <- findUserTokenByUuid uuid
    validateMcpTokenOwner currentUser userToken
    deleteUserTokenByUuid uuid
    al_deleteMcpToken userToken

mcpAccessTokenExpirationSeconds :: Integer
mcpAccessTokenExpirationSeconds = mcpAccessTokenExpirationHours * nominalHourInSeconds

orOAuthError :: Monad m => McpOAuthError -> Maybe a -> McpOAuthM m a
orOAuthError oauthError = maybe (throwError oauthError) return
