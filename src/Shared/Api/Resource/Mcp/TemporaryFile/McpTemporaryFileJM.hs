module Shared.Api.Resource.Mcp.TemporaryFile.McpTemporaryFileJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.TemporaryFile.McpTemporaryFileDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON McpTemporaryFileDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON McpTemporaryFileDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpTemporaryFileDTO where
  jsonSchema = genericJsonSchema jsonOptions
