module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON KnowledgeModelPackageSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageSuggestion where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelPackageSuggestion where
  jsonSchema = genericJsonSchema jsonOptions
