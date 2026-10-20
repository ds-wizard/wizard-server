module Shared.Api.Resource.DocumentTemplate.DocumentTemplateWithCoordinateJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateWithCoordinate
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON DocumentTemplateWithCoordinate where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateWithCoordinate where
  toJSON = genericToJSON jsonOptions

instance JsonSchema DocumentTemplateWithCoordinate where
  jsonSchema = genericJsonSchema jsonOptions
