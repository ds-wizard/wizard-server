module Shared.Api.Resource.Locale.LocaleSimpleJM where

import Data.Aeson

import Shared.Model.Locale.LocaleSimple
import Shared.Util.Aeson

instance FromJSON LocaleSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleSimple where
  toJSON = genericToJSON jsonOptions
