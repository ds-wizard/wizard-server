module Shared.Api.Resource.Locale.LocaleJM where

import Data.Aeson

import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON LocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema LocaleDTO where
  jsonSchema = genericJsonSchema jsonOptions
