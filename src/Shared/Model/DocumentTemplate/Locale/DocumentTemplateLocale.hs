module Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateLocale = DocumentTemplateLocale
  { uuid :: U.UUID
  , name :: String
  , code :: String
  , documentTemplateUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
