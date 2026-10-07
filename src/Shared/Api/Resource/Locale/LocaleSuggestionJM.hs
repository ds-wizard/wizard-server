module Shared.Api.Resource.Locale.LocaleSuggestionJM where

import Data.Aeson

import Shared.Api.Resource.Locale.LocaleSuggestionDTO
import Shared.Util.Aeson

instance FromJSON LocaleSuggestionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleSuggestionDTO where
  toJSON = genericToJSON jsonOptions
