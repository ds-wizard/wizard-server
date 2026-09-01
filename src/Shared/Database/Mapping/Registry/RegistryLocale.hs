module Shared.Database.Mapping.Registry.RegistryLocale where

import Database.PostgreSQL.Simple

import Shared.Model.Registry.RegistryLocale

instance ToRow RegistryLocale

instance FromRow RegistryLocale
