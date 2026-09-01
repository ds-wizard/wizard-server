module Shared.Model.DocumentTemplate.DocumentTemplateSuggestion where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.SemVer2Tuple
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

data DocumentTemplateSuggestion = DocumentTemplateSuggestion
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , language :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , formats :: [DocumentTemplateFormatSimple]
  , locales :: [DocumentTemplateLocaleList]
  }
  deriving (Show, Eq, Generic)
