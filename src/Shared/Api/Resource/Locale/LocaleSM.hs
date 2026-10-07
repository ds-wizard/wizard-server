module Shared.Api.Resource.Locale.LocaleSM where

import Data.Swagger

import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleJM ()
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Util.Swagger

instance ToSchema LocaleDTO where
  declareNamedSchema = toSwagger localeDeDto
