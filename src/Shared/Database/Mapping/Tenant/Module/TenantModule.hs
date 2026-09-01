module Shared.Database.Mapping.Tenant.Module.TenantModule where

import Database.PostgreSQL.Simple

import Shared.Database.Mapping.Common ()
import Shared.Model.Tenant.Module.TenantModule

instance FromRow TenantModule

instance ToRow TenantModule
