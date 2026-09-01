module Shared.Model.Project.Detail.ProjectDetailPreview where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Project.Acl.ProjectPermDTO
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion (KnowledgeModelPackageSuggestion)
import Shared.Model.Project.Project

data ProjectDetailPreview = ProjectDetailPreview
  { uuid :: U.UUID
  , name :: String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackage :: KnowledgeModelPackageSuggestion
  , isTemplate :: Bool
  , permissions :: [ProjectPermDTO]
  , documentTemplateUuid :: Maybe U.UUID
  , format :: Maybe DocumentTemplateFormatSimple
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
