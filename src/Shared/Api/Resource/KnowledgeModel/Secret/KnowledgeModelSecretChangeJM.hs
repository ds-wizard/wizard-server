module Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Shared.Util.Aeson

instance FromJSON KnowledgeModelSecretChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelSecretChangeDTO where
  toJSON = genericToJSON jsonOptions
