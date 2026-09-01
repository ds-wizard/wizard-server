module Shared.Database.Mapping.Tenant.Config.TenantConfigRegistry where

import Database.PostgreSQL.Simple

import Shared.Database.Mapping.Common ()
import Shared.Model.Tenant.Config.WizardTenantConfig

instance FromRow TenantConfigRegistry

instance ToRow TenantConfigRegistry
