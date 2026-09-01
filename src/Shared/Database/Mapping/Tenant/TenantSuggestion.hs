module Shared.Database.Mapping.Tenant.TenantSuggestion where

import Database.PostgreSQL.Simple

import Shared.Model.Tenant.TenantSuggestion

instance FromRow TenantSuggestion
