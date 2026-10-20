module Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON KnowledgeModelLocaleList where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelLocaleList where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema KnowledgeModelLocaleList where
  jsonSchema = genericJsonSchema jsonOptions
