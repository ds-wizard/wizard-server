module Shared.Database.Mapping.Mcp.McpAuthorizationCode where

import Database.PostgreSQL.Simple

import Shared.Database.Mapping.Common ()
import Shared.Database.Mapping.Mcp.McpToolset ()
import Shared.Model.Mcp.McpAuthorizationCode

instance FromRow McpAuthorizationCode

instance ToRow McpAuthorizationCode
