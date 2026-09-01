module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Shared.Util.Aeson

instance FromJSON KnowledgeModelPackagePattern where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackagePattern where
  toJSON = genericToJSON jsonOptions
