module Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data MailTemplateLocaleDTO = MailTemplateLocaleDTO
  { uuid :: U.UUID
  , code :: String
  , name :: String
  , potVersion :: Maybe String
  , enabled :: Bool
  , defaultLocale :: Bool
  , official :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
