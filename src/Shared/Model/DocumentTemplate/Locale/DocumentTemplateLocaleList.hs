module Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList where

import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateLocaleList = DocumentTemplateLocaleList
  { uuid :: U.UUID
  , name :: String
  , code :: String
  }
  deriving (Show, Eq, Generic)
