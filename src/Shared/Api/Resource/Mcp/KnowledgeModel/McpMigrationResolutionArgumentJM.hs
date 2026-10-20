module Shared.Api.Resource.Mcp.KnowledgeModel.McpMigrationResolutionArgumentJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.KnowledgeModel.McpMigrationResolutionArgumentDTO
import Shared.Util.JsonSchema

instance FromJSON McpMigrationResolutionArgumentDTO where
  parseJSON = withText "action" $ \case
    "apply" -> return ApplyMcpMigrationResolution
    "reject" -> return RejectMcpMigrationResolution
    "apply_all" -> return ApplyAllMcpMigrationResolution
    _ -> fail "use one of apply, reject, apply_all"

instance JsonSchema McpMigrationResolutionArgumentDTO where
  jsonSchema _ = enumSchema ["apply", "reject", "apply_all"]
