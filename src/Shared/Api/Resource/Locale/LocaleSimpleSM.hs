module Shared.Api.Resource.Locale.LocaleSimpleSM where

import Data.Swagger

import Shared.Api.Resource.Locale.LocaleSimpleJM ()
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Model.Locale.LocaleSimple
import Shared.Util.Swagger

instance ToSchema LocaleSimple where
  declareNamedSchema = toSwagger localeNlSimple
