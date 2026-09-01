module Shared.Api.Resource.Registry.RegistryConfirmationSM where

import Data.Swagger

import Shared.Api.Resource.Registry.RegistryConfirmationDTO
import Shared.Api.Resource.Registry.RegistryConfirmationJM ()
import Shared.Database.Migration.Development.Registry.Data.Registries
import Shared.Util.Swagger

instance ToSchema RegistryConfirmationDTO where
  declareNamedSchema = toSwagger registryConfirmation
