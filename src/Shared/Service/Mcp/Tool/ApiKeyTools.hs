module Shared.Service.Mcp.Tool.ApiKeyTools where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.UserToken.ApiKeyCreateDTO
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Api.Resource.UserToken.UserTokenListJM ()
import Shared.Constant.Mcp
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
import Shared.Model.AuditLog.AuditLogSource
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Model.User.UserToken
import Shared.Model.User.UserTokenList
import Shared.Service.Mcp.McpFlowService (deleteMcpTokenForCurrentUser)
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.UserToken.UserTokenService (getTokens)
import Shared.Util.Date (getCurrentTimeInMicroseconds)
import Shared.Util.JsonSchema

apiKeyTools :: WizardRequestContextC s m => (ApiKeyCreateDTO -> Maybe String -> m UserTokenDTO) -> (U.UUID -> m ()) -> [McpTool m]
apiKeyTools createApiKey deleteToken = [listApiKeysTool, createApiKeyTool createApiKey, deleteApiKeyTool deleteToken, listMcpConnectionsTool, deleteMcpConnectionTool]

listApiKeysTool :: WizardRequestContextC s m => McpTool m
listApiKeysTool =
  McpTool
    { name = "list_api_keys"
    , title = "List API keys"
    , description = "List your API keys with their names and expiration; their values are shown only once, when create_api_key creates them"
    , inputSchema = argumentsSchema (pure ())
    , outputSchema = Just $ objectSchema ["apiKeys"] [("apiKeys", arraySchema (jsonSchema (Proxy :: Proxy UserTokenList)))]
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments (pure ()) . const $ (\apiKeys -> object ["apiKeys" .= apiKeys]) <$> getTokens ApiKeyUserTokenType Nothing
    }

createApiKeyTool :: forall s m. WizardRequestContextC s m => (ApiKeyCreateDTO -> Maybe String -> m UserTokenDTO) -> McpTool m
createApiKeyTool createApiKey =
  McpTool
    { name = "create_api_key"
    , title = "Create API key"
    , description =
        "Create an API key that acts as you in the API until it expires (expires_at, in 30 days by default, at most "
          ++ show mcpApiKeyMaxExpirationDays
          ++ " days from now); its token is returned only by this call, so hand it over to the user right away"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ objectSchema ["name", "token", "expiresAt"] [("name", stringSchema), ("token", stringSchema), ("expiresAt", formatSchema "date-time")]
    , annotations = writeAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments (uncurry (createMcpApiKey createApiKey))
    }
  where
    arguments =
      (,)
        <$> requiredArgument "name" "The name of the API key"
        <*> optionalArgument "expires_at" "When the API key expires"
        :: Arguments (String, Maybe UTCTime)

createMcpApiKey :: WizardRequestContextC s m => (ApiKeyCreateDTO -> Maybe String -> m UserTokenDTO) -> String -> Maybe UTCTime -> m Value
createMcpApiKey createApiKey name mExpiresAt = do
  now <- liftIO getCurrentTimeInMicroseconds
  let expiresAt = fromMaybe (addUTCTime mcpApiKeyDefaultExpiration now) mExpiresAt
  unless (expiresAt > now && expiresAt <= addUTCTime (fromIntegral mcpApiKeyMaxExpirationDays * nominalDay) now) $
    throwError . UserError $
      _ERROR_SERVICE_MCP__API_KEY_EXPIRATION mcpApiKeyMaxExpirationDays
  toCreatedApiKey name <$> createApiKey (ApiKeyCreateDTO name expiresAt) (Just "MCP")

toCreatedApiKey :: String -> UserTokenDTO -> Value
toCreatedApiKey name (UserTokenDTO token expiresAt) = object ["name" .= name, "token" .= token, "expiresAt" .= expiresAt]
toCreatedApiKey name _ = object ["name" .= name]

deleteApiKeyTool :: WizardRequestContextC s m => (U.UUID -> m ()) -> McpTool m
deleteApiKeyTool deleteToken =
  McpTool
    { name = "delete_api_key"
    , title = "Delete API key"
    , description = "Delete one of your API keys (list_api_keys shows them); everything using it loses access at once. It cannot be undone"
    , inputSchema = argumentsSchema apiKeyUuidArgument
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments apiKeyUuidArgument $ \uuid -> object [] <$ deleteApiKey deleteToken uuid
    }

deleteApiKey :: WizardRequestContextC s m => (U.UUID -> m ()) -> U.UUID -> m ()
deleteApiKey deleteToken uuid = do
  apiKeys <- getTokens ApiKeyUserTokenType Nothing
  unless (any ((== uuid) . (.uuid)) apiKeys) $
    throwError . NotExistsError $
      _ERROR_DATABASE__ENTITY_NOT_FOUND "api_key" [("uuid", U.toString uuid)]
  deleteToken uuid

apiKeyUuidArgument :: Arguments U.UUID
apiKeyUuidArgument = requiredArgument "api_key_uuid" "The API key"

listMcpConnectionsTool :: WizardRequestContextC s m => McpTool m
listMcpConnectionsTool =
  McpTool
    { name = "list_mcp_connections"
    , title = "List MCP connections"
    , description = "List the MCP clients you have connected, with their names and when their grant expires; currentSession marks this connection"
    , inputSchema = argumentsSchema (pure ())
    , outputSchema = Just $ objectSchema ["connections"] [("connections", arraySchema (jsonSchema (Proxy :: Proxy UserTokenList)))]
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION]
    , toolset = Nothing
    , transactionState = NoTransaction
    , run = withArguments (pure ()) . const $ (\connections -> object ["connections" .= connections]) <$> getMcpConnections
    }

getMcpConnections :: WizardRequestContextC s m => m [UserTokenList]
getMcpConnections = do
  source <- asks (.auditLogSource')
  getTokens McpUserTokenType (if source.sType == McpAuditLogSourceType then source.uuid else Nothing)

deleteMcpConnectionTool :: WizardRequestContextC s m => McpTool m
deleteMcpConnectionTool =
  McpTool
    { name = "delete_mcp_connection"
    , title = "Delete MCP connection"
    , description = "Revoke one of your MCP connections (list_mcp_connections shows them); the client loses access at once and has to be connected again. It cannot be undone"
    , inputSchema = argumentsSchema connectionUuidArgument
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION]
    , toolset = Nothing
    , transactionState = Transactional
    , run = withArguments connectionUuidArgument $ \uuid -> object [] <$ deleteMcpTokenForCurrentUser uuid
    }

connectionUuidArgument :: Arguments U.UUID
connectionUuidArgument = requiredArgument "connection_uuid" "The MCP connection"
