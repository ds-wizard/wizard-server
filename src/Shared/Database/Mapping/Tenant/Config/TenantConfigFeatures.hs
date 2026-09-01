module Shared.Database.Mapping.Tenant.Config.TenantConfigFeatures where

import Database.PostgreSQL.Simple

import Shared.Model.Tenant.Config.TenantConfig

instance FromRow TenantConfigFeatures

instance ToRow TenantConfigFeatures
