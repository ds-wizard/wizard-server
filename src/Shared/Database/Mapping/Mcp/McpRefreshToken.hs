module Shared.Database.Mapping.Mcp.McpRefreshToken where

import Database.PostgreSQL.Simple

import Shared.Database.Mapping.Common ()
import Shared.Database.Mapping.Mcp.McpToolset ()
import Shared.Model.Mcp.McpRefreshToken

instance FromRow McpRefreshToken

instance ToRow McpRefreshToken
