module Shared.Api.Resource.Mcp.Project.McpRepliesChangeResultJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Project.McpRepliesChangeResultDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON McpRepliesChangeResultDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpRepliesChangeResultDTO where
  jsonSchema = genericJsonSchema jsonOptions
