module Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Util.Aeson

instance FromJSON KnowledgeModelSecret where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelSecret where
  toJSON = genericToJSON jsonOptions
