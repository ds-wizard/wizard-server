module Shared.Database.Mapping.Tenant.Config.TenantConfigMail where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Database.Mapping.Common ()
import Shared.Model.Tenant.Config.TenantConfig

instance FromRow TenantConfigMail where
  fromRow = do
    tenantUuid <- field
    configUuid <- field
    createdAt <- field
    updatedAt <- field
    customTemplates <- field
    return $ TenantConfigMail {..}

instance ToRow TenantConfigMail where
  toRow TenantConfigMail {..} =
    [ toField tenantUuid
    , toField configUuid
    , toField createdAt
    , toField updatedAt
    , toField customTemplates
    ]
