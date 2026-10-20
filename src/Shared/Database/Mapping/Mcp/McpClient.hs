module Shared.Database.Mapping.Mcp.McpClient where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Database.Mapping.Common ()
import Shared.Model.Mcp.McpClient

instance ToRow McpClient where
  toRow McpClient {..} =
    [ toField uuid
    , toField clientId
    , toField clientSecret
    , toField clientName
    , toJSONField redirectUris
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    , toField tokenEndpointAuthMethod
    ]

instance FromRow McpClient where
  fromRow = do
    uuid <- field
    clientId <- field
    clientSecret <- field
    clientName <- field
    redirectUris <- fieldWith fromJSONField
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    tokenEndpointAuthMethod <- field
    return $ McpClient {..}
