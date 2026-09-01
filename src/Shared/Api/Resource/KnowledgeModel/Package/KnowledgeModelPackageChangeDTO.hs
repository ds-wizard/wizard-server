module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO where

import GHC.Generics

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

data KnowledgeModelPackageChangeDTO = KnowledgeModelPackageChangeDTO
  { phase :: KnowledgeModelPackagePhase
  , public :: Bool
  }
  deriving (Show, Eq, Generic)
