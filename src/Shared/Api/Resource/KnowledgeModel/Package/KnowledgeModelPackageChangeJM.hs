module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Shared.Util.Aeson

instance FromJSON KnowledgeModelPackageChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageChangeDTO where
  toJSON = genericToJSON jsonOptions
