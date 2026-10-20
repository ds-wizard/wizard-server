module Shared.Api.Resource.Locale.Registry.RegistryLocaleJM where

import Data.Aeson

import Shared.Api.Resource.Locale.Registry.RegistryLocaleDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON RegistryLocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RegistryLocaleDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema RegistryLocaleDTO where
  jsonSchema = genericJsonSchema jsonOptions
