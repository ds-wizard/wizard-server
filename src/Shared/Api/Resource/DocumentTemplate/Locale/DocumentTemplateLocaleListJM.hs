module Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON DocumentTemplateLocaleList where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateLocaleList where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema DocumentTemplateLocaleList where
  jsonSchema = genericJsonSchema jsonOptions
