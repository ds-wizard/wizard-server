module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO where

import GHC.Generics

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

data DocumentTemplateDraftChangeDTO = DocumentTemplateDraftChangeDTO
  { name :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , language :: String
  , formats :: [DocumentTemplateFormatDTO]
  }
  deriving (Show, Eq, Generic)
