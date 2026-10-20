module Shared.Api.Resource.Mcp.Toolset.McpToolsetSM where

import Data.Aeson (Value (..))
import Data.Swagger
import qualified Data.Text as T

import Shared.Model.Mcp.McpToolset

instance ToParamSchema McpToolset where
  toParamSchema _ = mempty {_paramSchemaType = Just SwaggerString, _paramSchemaEnum = Just (fmap (String . T.pack . mcpToolsetName) allMcpToolsets)}
