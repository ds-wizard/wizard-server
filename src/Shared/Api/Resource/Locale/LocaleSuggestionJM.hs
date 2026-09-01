module Shared.Api.Resource.Locale.LocaleSuggestionJM where

import Data.Aeson

import Shared.Model.Locale.LocaleSuggestion
import Shared.Util.Aeson

instance FromJSON LocaleSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleSuggestion where
  toJSON = genericToJSON jsonOptions
