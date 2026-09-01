module Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList

data DocumentTemplateSuggestionDTO = DocumentTemplateSuggestionDTO
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , description :: String
  , language :: String
  , formats :: [DocumentTemplateFormatSimple]
  , locales :: [DocumentTemplateLocaleList]
  }
  deriving (Show, Eq, Generic)
