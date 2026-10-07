module Shared.Api.Resource.Locale.Registry.RegistryLocaleSM where

import Data.Swagger

import Shared.Api.Resource.Locale.Registry.RegistryLocaleDTO
import Shared.Api.Resource.Locale.Registry.RegistryLocaleJM ()
import Shared.Database.Migration.Development.Registry.Data.RegistryLocales
import Shared.Util.Swagger

instance ToSchema RegistryLocaleDTO where
  declareNamedSchema = toSwagger deRegistryLocaleDto
