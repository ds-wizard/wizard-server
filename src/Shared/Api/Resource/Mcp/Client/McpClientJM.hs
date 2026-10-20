module Shared.Api.Resource.Mcp.Client.McpClientJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Client.McpClientDTO
import Shared.Util.Aeson

instance FromJSON McpClientDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON McpClientDTO where
  toJSON = genericToJSON jsonOptions
