module Shared.Database.Mapping.Registry.RegistryMailTemplateLocale where

import Database.PostgreSQL.Simple

import Shared.Model.Registry.RegistryMailTemplateLocale

instance ToRow RegistryMailTemplateLocale

instance FromRow RegistryMailTemplateLocale
