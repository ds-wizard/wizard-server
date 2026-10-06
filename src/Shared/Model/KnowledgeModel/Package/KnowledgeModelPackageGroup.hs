module Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageGroup where

import GHC.Generics

data KnowledgeModelPackageGroup = KnowledgeModelPackageGroup
  { id :: String
  , versions :: String
  }
  deriving (Show, Eq, Generic)
