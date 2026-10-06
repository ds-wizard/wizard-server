module Shared.Database.Mapping.Plugin.TenantPluginSettings where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.Plugin.TenantPluginSettings

instance ToRow TenantPluginSettings where
  toRow TenantPluginSettings {..} =
    [ toField tenantUuid
    , toField pluginUuid
    , toJSONField values
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow TenantPluginSettings where
  fromRow = do
    tenantUuid <- field
    pluginUuid <- field
    values <- fieldWith fromJSONField
    createdAt <- field
    updatedAt <- field
    return $ TenantPluginSettings {..}
