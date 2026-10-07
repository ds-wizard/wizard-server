module Shared.Api.Resource.Locale.Registry.RegistryLocalePullSM where

import Data.Swagger

import Shared.Api.Resource.Locale.Registry.RegistryLocalePullDTO
import Shared.Api.Resource.Locale.Registry.RegistryLocalePullJM ()
import Shared.Database.Migration.Development.Registry.Data.RegistryLocales
import Shared.Util.Swagger

instance ToSchema RegistryLocalePullDTO where
  declareNamedSchema = toSwagger registryLocalePullDto
