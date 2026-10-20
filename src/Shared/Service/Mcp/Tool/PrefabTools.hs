module Shared.Service.Mcp.Tool.PrefabTools where

import Data.Aeson
import Data.Proxy

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.Prefab.PrefabJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.Prefab.Prefab
import Shared.Model.User.RolePermission
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Prefab.PrefabService
import Shared.Util.JsonSchema

prefabTools :: WizardRequestContextC s m => [McpTool m]
prefabTools = [listIntegrationPrefabsTool]

listIntegrationPrefabsTool :: WizardRequestContextC s m => McpTool m
listIntegrationPrefabsTool =
  McpTool
    { name = "list_integration_prefabs"
    , title = "List integration prefabs"
    , description = "List the prefabs of knowledge model integrations: ready-made integration definitions (content) to copy into an AddIntegrationEvent"
    , inputSchema = argumentsSchema pageableArguments
    , outputSchema = Just $ pageSchema "prefabs" (jsonSchema (Proxy :: Proxy Prefab))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = NoTransaction
    , run = withArguments pageableArguments $ \pageable ->
        toJSON . toPage "prefabs" pageable <$> getPrefabsFiltered [("type", "knowledge-model-integration")]
    }
