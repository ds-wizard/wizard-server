module Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON KnowledgeModelSecretChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelSecretChangeDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelSecretChangeDTO where
  jsonSchema = genericJsonSchema jsonOptions
