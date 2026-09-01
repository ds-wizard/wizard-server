module Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM ()
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Util.Aeson

instance FromJSON KnowledgeModelBundle where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelBundle where
  toJSON = genericToJSON jsonOptions
