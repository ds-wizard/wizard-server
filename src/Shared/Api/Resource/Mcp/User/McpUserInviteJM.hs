module Shared.Api.Resource.Mcp.User.McpUserInviteJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.User.McpUserInviteDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON McpUserInviteDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON McpUserInviteDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpUserInviteDTO where
  jsonSchema = genericJsonSchema jsonOptions
