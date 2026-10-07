module Shared.Api.Resource.Locale.LocaleSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.Locale.LocaleSuggestionDTO
import Shared.Api.Resource.Locale.LocaleSuggestionJM ()
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Util.Swagger

instance ToSchema LocaleSuggestionDTO where
  declareNamedSchema = toSwagger localeDeSuggestionDto
