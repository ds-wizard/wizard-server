module Specs.Service.Mcp.Tool.ApiKeyTools.ToolSpec (
  apiKeyToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.ApiKeyTools.CreateApiKey
import Specs.Service.Mcp.Tool.ApiKeyTools.DeleteApiKey
import Specs.Service.Mcp.Tool.ApiKeyTools.DeleteMcpConnection
import Specs.Service.Mcp.Tool.ApiKeyTools.ListApiKeys
import Specs.Service.Mcp.Tool.ApiKeyTools.ListMcpConnections

apiKeyToolsSpec :: RequestContext -> SpecWith ((), Application)
apiKeyToolsSpec requestContext = do
  list_api_keys requestContext
  create_api_key requestContext
  delete_api_key requestContext
  list_mcp_connections requestContext
  delete_mcp_connection requestContext
