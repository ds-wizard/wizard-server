module Shared.Service.Mcp.McpFlowValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlphaNum, isAscii, toLower)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.UUID as U
import Network.HTTP.Types.URI (urlDecode)
import Network.URI (URI, parseAbsoluteURI, uriAuthority, uriFragment, uriRegName, uriScheme)

import Shared.Api.Resource.Mcp.Token.McpTokenCreateDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Localization.Locale
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.OpenId.Public
import Shared.Localization.Messages.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpAuthorizationCode
import Shared.Model.Mcp.McpClient
import Shared.Model.Mcp.McpOAuthError
import Shared.Model.Mcp.McpRefreshToken
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.User.UserToken
import Shared.Util.String (splitOn)

validateRedirectUri :: WizardRequestContextC s m => McpClient -> String -> m ()
validateRedirectUri client redirectUri =
  if redirectUri `elem` client.redirectUris
    then return ()
    else throwError . UserError . _ERROR_VALIDATION__OPENID_CLIENT_ABSENCE $ "redirect_uri not registered for client " ++ client.clientId

validateCodeChallenge :: WizardRequestContextC s m => Maybe String -> Maybe String -> m ()
validateCodeChallenge Nothing Nothing = return ()
validateCodeChallenge (Just challenge) (Just "S256")
  | length challenge == 43 && all isUnreservedChar challenge = return ()
  | otherwise = throwError . UserError $ _ERROR_VALIDATION__MCP_CODE_CHALLENGE
validateCodeChallenge _ mMethod = throwError . UserError . _ERROR_VALIDATION__MCP_CODE_CHALLENGE_METHOD $ fromMaybe "" mMethod

validateMcpPermission :: WizardRequestContextC s m => User -> m ()
validateMcpPermission user =
  if _MCP_READ_ROLE_PERMISSION `elem` user.role.permissions
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__MISSING_PERMISSION _MCP_READ_ROLE_PERMISSION

validateMcpTokenOwner :: WizardRequestContextC s m => UserDTO -> UserToken -> m ()
validateMcpTokenOwner currentUser userToken
  | userToken.userUuid == currentUser.uuid && userToken.tType == McpUserTokenType = return ()
  | otherwise =
      throwError . NotExistsError $
        _ERROR_DATABASE__ENTITY_NOT_FOUND "user_token" [("tenant_uuid", U.toString userToken.tenantUuid), ("uuid", U.toString userToken.uuid)]

validateMcpGrantPermission :: User -> Either McpOAuthError ()
validateMcpGrantPermission user
  | _MCP_READ_ROLE_PERMISSION `elem` user.role.permissions = Right ()
  | otherwise = Left . invalidGrant . toLocaleMessage $ _ERROR_VALIDATION__MCP_MISSING_PERMISSION _MCP_READ_ROLE_PERMISSION

validateTokenEndpointAuthMethod :: Maybe String -> Either McpOAuthError String
validateTokenEndpointAuthMethod Nothing = Right "client_secret_post"
validateTokenEndpointAuthMethod (Just method)
  | method `elem` ["client_secret_post", "client_secret_basic", "none"] = Right method
  | otherwise = Left . invalidClientMetadata $ "Unsupported token_endpoint_auth_method: " ++ method

validateRedirectUris :: [String] -> Either McpOAuthError ()
validateRedirectUris [] = Left $ invalidRedirectUri "At least one redirect_uri is required"
validateRedirectUris redirectUris
  | length redirectUris > 10 = Left $ invalidRedirectUri "At most 10 redirect_uris are allowed"
  | otherwise = traverse_ validateRedirectUriFormat redirectUris

validateRedirectUriFormat :: String -> Either McpOAuthError ()
validateRedirectUriFormat redirectUri =
  case parseAbsoluteURI redirectUri of
    Just uri | null (uriFragment uri) && isAllowedRedirectScheme uri -> Right ()
    _ -> Left . invalidRedirectUri $ "Invalid redirect_uri: " ++ redirectUri

isAllowedRedirectScheme :: URI -> Bool
isAllowedRedirectScheme uri =
  uriScheme uri == "https:" || (uriScheme uri == "http:" && maybe False (isLoopbackHost . uriRegName) (uriAuthority uri))

isLoopbackHost :: String -> Bool
isLoopbackHost host = map toLower host `elem` ["localhost", "127.0.0.1", "[::1]"]

validateClientName :: Maybe String -> Either McpOAuthError ()
validateClientName mClientName
  | maybe 0 length mClientName > 255 = Left $ invalidClientMetadata "client_name is too long"
  | otherwise = Right ()

resolveClientCredentials :: Maybe String -> McpTokenCreateDTO -> Either McpOAuthError (String, Maybe String)
resolveClientCredentials mAuthHeader reqDto =
  case (mAuthHeader >>= stripBasicScheme, reqDto.clientId) of
    (Just encoded, mFormClientId) -> do
      (clientId, clientSecret) <- maybe (Left $ invalidClient "Invalid Authorization header") Right (separateBasicCredentials encoded)
      when (maybe False (/= clientId) mFormClientId) . Left $ invalidRequest "client_id does not match the Authorization header"
      return (clientId, Just clientSecret)
    (Nothing, Just clientId) -> Right (clientId, reqDto.clientSecret)
    (Nothing, Nothing) -> Left $ invalidClient "Missing client_id"

validateClientSecret :: McpClient -> Maybe String -> Either McpOAuthError ()
validateClientSecret client mClientSecret
  | client.tokenEndpointAuthMethod == "none" = Right ()
  | mClientSecret == Just client.clientSecret = Right ()
  | otherwise = Left $ invalidClient "Invalid client credentials"

validateAuthorizationCode :: McpAuthorizationCode -> McpClient -> Maybe String -> UTCTime -> Either McpOAuthError ()
validateAuthorizationCode authCode client mRedirectUri now
  | authCode.clientId /= client.clientId = Left $ invalidGrant "Authorization code does not belong to the client"
  | mRedirectUri /= Just authCode.redirectUri = Left $ invalidGrant "redirect_uri mismatch"
  | authCode.expiresAt <= now = Left $ invalidGrant "Authorization code expired"
  | otherwise = Right ()

validateCodeVerifier :: McpAuthorizationCode -> McpClient -> Maybe String -> Either McpOAuthError ()
validateCodeVerifier authCode client mCodeVerifier =
  case (authCode.codeChallenge, mCodeVerifier) of
    (Nothing, _)
      | client.tokenEndpointAuthMethod == "none" -> Left $ invalidRequest "A public client must use PKCE"
      | otherwise -> Right ()
    (Just _, Nothing) -> Left $ invalidRequest "Missing code_verifier"
    (Just codeChallenge, Just codeVerifier)
      | not (isValidCodeVerifier codeVerifier) -> Left $ invalidRequest "Invalid code_verifier format"
      | hashSHA256Base64Url codeVerifier == codeChallenge -> Right ()
      | otherwise -> Left $ invalidGrant "Invalid code_verifier"

isValidCodeVerifier :: String -> Bool
isValidCodeVerifier codeVerifier = length codeVerifier >= 43 && length codeVerifier <= 128 && all isUnreservedChar codeVerifier

isUnreservedChar :: Char -> Bool
isUnreservedChar c = (isAscii c && isAlphaNum c) || c `elem` "-._~"

validateRefreshToken :: McpRefreshToken -> McpClient -> UTCTime -> Either McpOAuthError ()
validateRefreshToken refreshToken client now
  | refreshToken.clientId /= client.clientId = Left $ invalidGrant "Refresh token does not belong to the client"
  | refreshToken.expiresAt <= now = Left $ invalidGrant "Refresh token expired"
  | otherwise = Right ()

stripBasicScheme :: String -> Maybe String
stripBasicScheme headerValue =
  case splitOn " " headerValue of
    [scheme, encoded] | map toLower scheme == "basic" -> Just encoded
    _ -> Nothing

separateBasicCredentials :: String -> Maybe (String, String)
separateBasicCredentials encoded =
  case break (== ':') (BS.unpack . B64.decodeBase64Lenient . BS.pack $ encoded) of
    (clientId, ':' : clientSecret) -> Just (urlDecodeString clientId, urlDecodeString clientSecret)
    _ -> Nothing

urlDecodeString :: String -> String
urlDecodeString = BS.unpack . urlDecode False . BS.pack

hashSHA256Base64Url :: String -> String
hashSHA256Base64Url text =
  let digest :: Digest SHA256
      digest = hash . TE.encodeUtf8 . T.pack $ text
   in BS.unpack $ convertToBase Base64URLUnpadded digest
