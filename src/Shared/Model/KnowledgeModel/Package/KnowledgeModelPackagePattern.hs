module Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern where

import GHC.Generics

data KnowledgeModelPackagePattern = KnowledgeModelPackagePattern
  { id :: Maybe String
  , minVersion :: Maybe String
  , maxVersion :: Maybe String
  }
  deriving (Show, Eq, Generic)
