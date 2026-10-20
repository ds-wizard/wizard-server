module Shared.Api.Resource.Mcp.Registry.McpCatalogKindArgumentJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Registry.McpCatalogKindArgumentDTO
import Shared.Util.JsonSchema

instance FromJSON McpCatalogKindArgumentDTO where
  parseJSON value = do
    kind <- parseJSON value
    if kind `elem` catalogKinds then return (McpCatalogKindArgumentDTO kind) else fail ("use one of " ++ unwords catalogKinds)

instance JsonSchema McpCatalogKindArgumentDTO where
  jsonSchema _ = enumSchema catalogKinds

catalogKinds :: [String]
catalogKinds = ["knowledgeModel", "documentTemplate"]
