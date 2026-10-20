module Shared.Database.Mapping.Mcp.McpToolset where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common ()
import Shared.Model.Mcp.McpToolset

instance ToField [McpToolset] where
  toField = toField . fmap mcpToolsetName

instance FromField [McpToolset] where
  fromField f mdata = do
    names <- fromField f mdata
    maybe (returnError ConversionFailed f (unwords names)) return (traverse parseMcpToolset names)
