module Shared.Service.Mcp.Tool.TenantContextTools where

import Data.Aeson (ToJSON, Value, toJSON)
import Data.Proxy

import Shared.Api.Resource.Mcp.TenantContext.TenantContextDTO
import Shared.Api.Resource.Mcp.TenantContext.TenantContextJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.User.RolePermission
import Shared.Service.Mcp.TenantContext.TenantContextService
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Util.JsonSchema

tenantContextTools :: (WizardRequestContextC s m, ToJSON usage, JsonSchema usage) => m usage -> [McpTool m]
tenantContextTools getUsage = [getTenantContextTool getUsage]

getTenantContextTool :: forall s m usage. (WizardRequestContextC s m, ToJSON usage, JsonSchema usage) => m usage -> McpTool m
getTenantContextTool getUsage =
  McpTool
    { name = "get_tenant_context"
    , title = "Get tenant context"
    , description =
        "Get who you are, your effective permissions, whether the tenant has one or more workspaces, the workspaces you can reach with your role in each, "
          ++ "the plan limits and usage (with organizationSettings.manage) and the effective settings of the organization or of a chosen workspace. "
          ++ "Call it first to learn where and what you may change."
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ withProperties [("usage", nullableSchema (jsonSchema (Proxy :: Proxy usage)))] (jsonSchema (Proxy :: Proxy TenantContextDTO))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Nothing
    , transactionState = Transactional
    , run = withArguments arguments $ \inScope -> inScope (toJSON <$> getTenantContext (toJSON <$> getUsage))
    }
  where
    arguments = scopeArguments :: Arguments (m Value -> m Value)
