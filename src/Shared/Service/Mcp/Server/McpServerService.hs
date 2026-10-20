module Shared.Service.Mcp.Server.McpServerService where

import Control.Monad (filterM)
import Control.Monad.Reader (asks)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import Shared.Api.Resource.Mcp.JsonRpc.JsonRpcRequestDTO
import Shared.Api.Resource.Mcp.JsonRpc.JsonRpcRequestJM ()
import Shared.Api.Resource.Mcp.JsonRpc.JsonRpcResponseDTO
import Shared.Constant.Mcp
import Shared.Localization.Locale
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpCaller
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Service.Mcp.Server.McpOutputLimit (toToolError)
import Shared.Service.Mcp.Server.McpToolService

supportedProtocolVersions :: [String]
supportedProtocolVersions = ["2025-11-25", "2025-06-18", "2025-03-26"]

handleMcpRequest :: WizardRequestContextC s m => [McpTool m] -> Maybe U.UUID -> Maybe [McpToolset] -> Value -> m (Maybe JsonRpcResponseDTO)
handleMcpRequest tools mTokenUuid mToolsets body = do
  checkPermission _MCP_READ_ROLE_PERMISSION
  caller <- findMcpCaller mTokenUuid mToolsets
  case fromJSON body of
    Error _ -> return . Just $ JsonRpcResponseDTO Null (Left invalidRequest)
    Success request -> traverse (dispatch tools caller request) request.id

dispatch :: WizardRequestContextC s m => [McpTool m] -> McpCaller -> JsonRpcRequestDTO -> Value -> m JsonRpcResponseDTO
dispatch tools caller request requestId = JsonRpcResponseDTO requestId <$> handleMethod tools caller request.method request.params

handleMethod :: WizardRequestContextC s m => [McpTool m] -> McpCaller -> String -> Value -> m (Either JsonRpcErrorDTO Value)
handleMethod _ _ "initialize" params = Right <$> initialize params
handleMethod _ _ "ping" _ = return . Right $ object []
handleMethod tools caller "tools/list" _ = Right <$> listTools tools caller.toolsets
handleMethod tools caller "tools/call" params = callTool tools caller params
handleMethod _ _ method _ = return . Left $ methodNotFound method

initialize :: WizardRequestContextC s m => Value -> m Value
initialize params = do
  buildInfoConfig <- asks (.buildInfoConfig')
  let protocolVersion = negotiateProtocolVersion $ parseMaybe (withObject "params" (.: "protocolVersion")) params
  return $
    object
      [ "protocolVersion" .= protocolVersion
      , "capabilities" .= object ["tools" .= object []]
      , "serverInfo" .= object ["name" .= ("FAIR Wizard" :: String), "version" .= buildInfoConfig.version]
      , "instructions" .= mcpInstructions
      ]

listTools :: WizardRequestContextC s m => [McpTool m] -> [McpToolset] -> m Value
listTools tools toolsets = do
  allowedTools <- filterM (fmap isRight . toolAccess toolsets) tools
  return $ object ["tools" .= fmap toToolDTO allowedTools]

negotiateProtocolVersion :: Maybe String -> String
negotiateProtocolVersion (Just requested)
  | requested `elem` supportedProtocolVersions = requested
negotiateProtocolVersion _ = head supportedProtocolVersions

callTool :: WizardRequestContextC s m => [McpTool m] -> McpCaller -> Value -> m (Either JsonRpcErrorDTO Value)
callTool tools caller params =
  case parseMaybe (withObject "params" (.: "name")) params of
    Nothing -> return . Left . invalidParams $ toLocaleMessage _ERROR_SERVICE_MCP__MISSING_TOOL_NAME
    Just name -> maybe (return . Left . invalidParams . toLocaleMessage $ _ERROR_SERVICE_MCP__UNKNOWN_TOOL name) (runToolCall caller params) (findMcpTool tools name)

runToolCall :: WizardRequestContextC s m => McpCaller -> Value -> McpTool m -> m (Either JsonRpcErrorDTO Value)
runToolCall caller params tool = do
  let arguments = fromMaybe (object []) (parseMaybe (withObject "params" (.: "arguments")) params)
  access <- toolAccess caller.toolsets tool
  Right <$> case access >> first toToolError (parseToolCall tool arguments) of
    Left toolError -> return toolError
    Right (limit, action) -> withMcpAuditLogSource caller tool.name (runMcpTool tool.transactionState limit action)

invalidRequest :: JsonRpcErrorDTO
invalidRequest = JsonRpcErrorDTO (-32600) (toLocaleMessage _ERROR_SERVICE_MCP__INVALID_REQUEST)

methodNotFound :: String -> JsonRpcErrorDTO
methodNotFound method = JsonRpcErrorDTO (-32601) (toLocaleMessage $ _ERROR_SERVICE_MCP__METHOD_NOT_FOUND method)

internalError :: JsonRpcErrorDTO
internalError = JsonRpcErrorDTO (-32603) (toLocaleMessage _ERROR_SERVICE_MCP__INTERNAL_ERROR)

invalidParams :: String -> JsonRpcErrorDTO
invalidParams = JsonRpcErrorDTO (-32602)
