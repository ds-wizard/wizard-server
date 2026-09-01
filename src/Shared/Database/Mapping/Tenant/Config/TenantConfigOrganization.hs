module Shared.Database.Mapping.Tenant.Config.TenantConfigOrganization where

import Database.PostgreSQL.Simple

import Shared.Database.Mapping.Common ()
import Shared.Model.Tenant.Config.WizardTenantConfig

instance FromRow TenantConfigOrganization

instance ToRow TenantConfigOrganization
