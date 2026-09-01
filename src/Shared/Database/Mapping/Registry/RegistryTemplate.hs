module Shared.Database.Mapping.Registry.RegistryTemplate where

import Database.PostgreSQL.Simple

import Shared.Model.Registry.RegistryTemplate

instance ToRow RegistryTemplate

instance FromRow RegistryTemplate
