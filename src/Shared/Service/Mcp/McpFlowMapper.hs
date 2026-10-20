module Shared.Service.Mcp.McpFlowMapper where

import Data.Maybe (fromMaybe)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.UUID as U

import Shared.Api.Resource.Mcp.Client.McpClientDTO
import Shared.Api.Resource.Mcp.Code.McpCodeCreateDTO
import Shared.Api.Resource.Mcp.Metadata.McpMetadataDTO
import Shared.Api.Resource.Mcp.Metadata.McpProtectedResourceDTO
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateDTO
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationDTO
import Shared.Api.Resource.Mcp.Token.McpTokenDTO
import Shared.Model.Mcp.McpAuthorizationCode
import Shared.Model.Mcp.McpClient
import Shared.Model.Mcp.McpRefreshToken
import Shared.Model.Mcp.McpToolset
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Model.User.UserToken
import Shared.Service.Tenant.TenantMapper (tenantServerUrl)
import qualified Shared.Service.UserToken.UserTokenMapper as UserTokenMapper
import Shared.Util.String

toMcpClient :: U.UUID -> String -> String -> String -> McpClientRegistrationCreateDTO -> U.UUID -> UTCTime -> McpClient
toMcpClient uuid clientId clientSecret authMethod reqDto tenantUuid now =
  McpClient
    { uuid = uuid
    , clientId = clientId
    , clientSecret = clientSecret
    , clientName = reqDto.clientName
    , redirectUris = reqDto.redirectUris
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    , tokenEndpointAuthMethod = authMethod
    }

toMcpClientDTO :: McpClient -> McpClientDTO
toMcpClientDTO client = McpClientDTO {clientName = client.clientName, redirectUris = client.redirectUris}

toMcpClientRegistrationDTO :: McpClient -> McpClientRegistrationDTO
toMcpClientRegistrationDTO client =
  McpClientRegistrationDTO
    { clientId = client.clientId
    , clientSecret = if client.tokenEndpointAuthMethod == "none" then Nothing else Just client.clientSecret
    , clientIdIssuedAt = floor . utcTimeToPOSIXSeconds $ client.createdAt
    , clientSecretExpiresAt = 0
    , redirectUris = client.redirectUris
    , clientName = client.clientName
    , tokenEndpointAuthMethod = client.tokenEndpointAuthMethod
    }

mcpAuthorizationCodeExpirationSeconds :: NominalDiffTime
mcpAuthorizationCodeExpirationSeconds = 60

toMcpAuthorizationCode :: U.UUID -> String -> McpClient -> User -> McpCodeCreateDTO -> UTCTime -> McpAuthorizationCode
toMcpAuthorizationCode uuid code client user reqDto now =
  McpAuthorizationCode
    { uuid = uuid
    , code = code
    , clientId = client.clientId
    , userUuid = user.uuid
    , redirectUri = reqDto.redirectUri
    , expiresAt = addUTCTime mcpAuthorizationCodeExpirationSeconds now
    , tenantUuid = user.tenantUuid
    , createdAt = now
    , codeChallenge = reqDto.codeChallenge
    , codeChallengeMethod = reqDto.codeChallengeMethod
    , toolsets = fromMaybe allMcpToolsets reqDto.toolsets
    }

toMcpUserToken :: U.UUID -> User -> McpClient -> String -> UTCTime -> UTCTime -> String -> UserToken
toMcpUserToken uuid user client secret now expiresAt =
  UserTokenMapper.toUserToken
    uuid
    "MCP Access Token"
    McpUserTokenType
    user.uuid
    expiresAt
    secret
    (Just $ fromMaybe "MCP Client" client.clientName)
    Nothing
    user.tenantUuid
    now

renewMcpUserToken :: UserToken -> String -> UTCTime -> UserToken
renewMcpUserToken userToken tokenValue expiresAt = userToken {value = tokenValue, expiresAt = expiresAt}

toMcpRefreshToken :: U.UUID -> String -> Maybe String -> UserToken -> McpClient -> [McpToolset] -> UTCTime -> McpRefreshToken
toMcpRefreshToken uuid tokenHash mPreviousTokenHash userToken client toolsets now =
  McpRefreshToken
    { uuid = uuid
    , tokenHash = tokenHash
    , previousTokenHash = mPreviousTokenHash
    , userTokenUuid = userToken.uuid
    , clientId = client.clientId
    , tenantUuid = userToken.tenantUuid
    , expiresAt = userToken.expiresAt
    , createdAt = now
    , toolsets = toolsets
    }

toMcpTokenDTO :: String -> Integer -> String -> McpTokenDTO
toMcpTokenDTO accessToken expiresIn refreshToken =
  McpTokenDTO
    { accessToken = accessToken
    , tokenType = "Bearer"
    , expiresIn = expiresIn
    , refreshToken = refreshToken
    }

mcpIssuer :: Tenant -> String
mcpIssuer tenant = tenantServerUrl tenant ++ "/mcp"

toMcpProtectedResourceDTO :: Tenant -> McpProtectedResourceDTO
toMcpProtectedResourceDTO tenant =
  McpProtectedResourceDTO
    { resource = mcpIssuer tenant
    , authorizationServers = [mcpIssuer tenant]
    , bearerMethodsSupported = ["header"]
    , scopesSupported = []
    }

toMcpMetadataDTO :: Tenant -> McpMetadataDTO
toMcpMetadataDTO tenant =
  let issuer = mcpIssuer tenant
   in McpMetadataDTO
        { issuer = issuer
        , authorizationEndpoint = f' "%s/mcp" [tenant.clientUrl]
        , tokenEndpoint = f' "%s/oauth2/token" [issuer]
        , registrationEndpoint = f' "%s/oauth2/register" [issuer]
        , jwksUri = f' "%s/oauth2/keys" [issuer]
        , responseTypesSupported = ["code"]
        , responseModesSupported = ["query"]
        , grantTypesSupported = ["authorization_code", "refresh_token"]
        , tokenEndpointAuthMethodsSupported = ["client_secret_post", "client_secret_basic", "none"]
        , codeChallengeMethodsSupported = ["S256"]
        , scopesSupported = []
        , subjectTypesSupported = ["public"]
        , idTokenSigningAlgValuesSupported = ["RS256"]
        }
