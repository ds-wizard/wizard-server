module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateDraftList
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON DocumentTemplateDraftList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftList where
  toJSON = genericToJSON jsonOptions

instance JsonSchema DocumentTemplateDraftList where
  jsonSchema = genericJsonSchema jsonOptions
