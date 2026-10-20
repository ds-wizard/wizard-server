module Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON DocumentTemplateSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSimple where
  toJSON = genericToJSON jsonOptions

instance JsonSchema DocumentTemplateSimple where
  jsonSchema = genericJsonSchema jsonOptions
