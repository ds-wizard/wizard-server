module Shared.Api.Resource.Mcp.Workspace.McpWorkspaceMembersJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Workspace.McpWorkspaceMembersDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON McpWorkspaceMemberDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON McpWorkspaceMemberDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpWorkspaceMemberDTO where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON McpWorkspaceMembersDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON McpWorkspaceMembersDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpWorkspaceMembersDTO where
  jsonSchema = genericJsonSchema jsonOptions
