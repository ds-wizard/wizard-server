module Shared.Api.Resource.Locale.LocaleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data LocaleDTO = LocaleDTO
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
