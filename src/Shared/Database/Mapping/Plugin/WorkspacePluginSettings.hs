module Shared.Database.Mapping.Plugin.WorkspacePluginSettings where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Null (..))

import Shared.Model.Plugin.WorkspacePluginSettings

instance ToRow WorkspacePluginSettings where
  toRow WorkspacePluginSettings {..} =
    [ toField workspaceUuid
    , toField pluginUuid
    , toField tenantUuid
    , toField enabled
    , maybe (toField Null) toJSONField values
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow WorkspacePluginSettings where
  fromRow = do
    workspaceUuid <- field
    pluginUuid <- field
    tenantUuid <- field
    enabled <- field
    values <- fieldWith (optionalField fromJSONField)
    createdAt <- field
    updatedAt <- field
    return $ WorkspacePluginSettings {..}
