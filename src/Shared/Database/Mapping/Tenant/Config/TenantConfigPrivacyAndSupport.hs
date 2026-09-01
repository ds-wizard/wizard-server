module Shared.Database.Mapping.Tenant.Config.TenantConfigPrivacyAndSupport where

import Database.PostgreSQL.Simple

import Shared.Model.Tenant.Config.WizardTenantConfig

instance ToRow TenantConfigPrivacyAndSupport

instance FromRow TenantConfigPrivacyAndSupport
