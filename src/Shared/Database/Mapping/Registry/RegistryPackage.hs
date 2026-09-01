module Shared.Database.Mapping.Registry.RegistryPackage where

import Database.PostgreSQL.Simple

import Shared.Model.Registry.RegistryPackage

instance ToRow RegistryPackage

instance FromRow RegistryPackage
