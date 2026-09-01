module Shared.Api.Resource.Registry.RegistryCreateSM where

import Data.Swagger

import Shared.Api.Resource.Registry.RegistryCreateDTO
import Shared.Api.Resource.Registry.RegistryCreateJM ()
import Shared.Database.Migration.Development.Registry.Data.Registries
import Shared.Util.Swagger

instance ToSchema RegistryCreateDTO where
  declareNamedSchema = toSwagger registryCreate
