module Shared.Service.Mcp.McpPluginService where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Shared.Api.Resource.Mcp.Plugin.McpPluginClient
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpToolset
import Shared.Service.Mcp.McpPluginMapper
import Shared.Service.Tenant.TenantHelper

getMcpPlugin :: WizardRequestContextC s m => BS.ByteString -> McpPluginClient -> Maybe [McpToolset] -> m (String, BSL.ByteString)
getMcpPlugin logo client mToolsets = do
  tenant <- getCurrentTenant
  return (toMcpPluginFileName client, toMcpPluginArchive logo client tenant mToolsets)
