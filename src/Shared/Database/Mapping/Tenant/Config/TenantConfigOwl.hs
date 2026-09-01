module Shared.Database.Mapping.Tenant.Config.TenantConfigOwl where

import Database.PostgreSQL.Simple

import Shared.Database.Mapping.Common ()
import Shared.Model.Tenant.Config.WizardTenantConfig

instance FromRow TenantConfigOwl

instance ToRow TenantConfigOwl
