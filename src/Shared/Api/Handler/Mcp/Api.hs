module Shared.Api.Handler.Mcp.Api where

import qualified Data.ByteString as BS
import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Mcp.Detail_AuthorizationServer_GET
import Shared.Api.Handler.Mcp.Detail_Clients_GET
import Shared.Api.Handler.Mcp.Detail_Codes_POST
import Shared.Api.Handler.Mcp.Detail_Keys_GET
import Shared.Api.Handler.Mcp.Detail_OpenIdConfiguration_GET
import Shared.Api.Handler.Mcp.Detail_Plugins_GET
import Shared.Api.Handler.Mcp.Detail_ProtectedResource_GET
import Shared.Api.Handler.Mcp.Detail_Register_POST
import Shared.Api.Handler.Mcp.Detail_Token_POST
import Shared.Api.Handler.Mcp.Detail_Tokens_DELETE
import Shared.Api.Handler.Mcp.Detail_Tokens_GET
import Shared.Api.Handler.Mcp.List_POST
import Shared.Api.Handler.WizardCommon
import Shared.Model.Mcp.McpTool

type McpAPI =
  Tags "MCP"
    :> ( List_POST
           :<|> Detail_ProtectedResource_GET
           :<|> Detail_AuthorizationServer_GET
           :<|> Detail_OpenIdConfiguration_GET
           :<|> Detail_Keys_GET
           :<|> Detail_Plugins_GET
           :<|> Detail_Clients_GET
           :<|> Detail_Register_POST
           :<|> Detail_Codes_POST
           :<|> Detail_Token_POST
           :<|> Detail_Tokens_GET
           :<|> Detail_Tokens_DELETE
       )

mcpApi :: Proxy McpAPI
mcpApi = Proxy

mcpServer :: WizardHandlerC s sm r rm => [McpTool rm] -> BS.ByteString -> ServerT McpAPI sm
mcpServer tools pluginLogo =
  list_POST tools
    :<|> detail_protectedResource_GET
    :<|> detail_authorizationServer_GET
    :<|> detail_openIdConfiguration_GET
    :<|> detail_keys_GET
    :<|> detail_plugins_GET pluginLogo
    :<|> detail_clients_GET
    :<|> detail_register_POST
    :<|> detail_codes_POST
    :<|> detail_token_POST
    :<|> detail_tokens_GET
    :<|> detail_tokens_DELETE
