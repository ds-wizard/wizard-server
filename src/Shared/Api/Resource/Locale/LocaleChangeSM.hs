module Shared.Api.Resource.Locale.LocaleChangeSM where

import Data.Swagger

import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Api.Resource.Locale.LocaleChangeJM ()
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Util.Swagger

instance ToSchema LocaleChangeDTO where
  declareNamedSchema = toSwagger localeDeChangeDto
