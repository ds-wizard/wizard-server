module Shared.Api.Resource.Mcp.Project.McpProjectDetailJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Project.McpProjectDetailDTO
import Shared.Api.Resource.Mcp.Project.McpProjectJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Api.Resource.Project.ProjectStateJM ()
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON McpProjectDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON McpProjectDetailDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpProjectDetailDTO where
  jsonSchema = genericJsonSchema jsonOptions
