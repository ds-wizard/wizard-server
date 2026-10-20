module Shared.Api.Resource.Registry.RegistryCatalogItemJM where

import Data.Aeson

import Shared.Api.Resource.Registry.RegistryCatalogItemDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON RegistryCatalogItemDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RegistryCatalogItemDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema RegistryCatalogItemDTO where
  jsonSchema = genericJsonSchema jsonOptions
