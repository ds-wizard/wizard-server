module Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON KnowledgeModelSecret where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelSecret where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelSecret where
  jsonSchema = genericJsonSchema jsonOptions
