module Shared.Database.Mapping.Registry.RegistryOrganization where

import Database.PostgreSQL.Simple

import Shared.Model.Registry.RegistryOrganization

instance ToRow RegistryOrganization

instance FromRow RegistryOrganization
