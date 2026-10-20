module Shared.Api.Resource.Mcp.Project.McpProjectJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Project.McpProjectDTO
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON McpProjectDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON McpProjectDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpProjectDTO where
  jsonSchema = genericJsonSchema jsonOptions
