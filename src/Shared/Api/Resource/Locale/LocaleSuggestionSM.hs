module Shared.Api.Resource.Locale.LocaleSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.Locale.LocaleSuggestionJM ()
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Model.Locale.LocaleSuggestion
import Shared.Util.Swagger

instance ToSchema LocaleSuggestion where
  declareNamedSchema = toSwagger localeNlSuggestion
