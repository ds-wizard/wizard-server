module Shared.Api.Resource.Registry.RegistryOrganizationSM where

import Data.Swagger

import Shared.Api.Resource.Registry.RegistryOrganizationJM ()
import Shared.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Shared.Model.Registry.RegistryOrganization
import Shared.Util.Swagger

instance ToSchema RegistryOrganization where
  declareNamedSchema = toSwagger globalRegistryOrganization
