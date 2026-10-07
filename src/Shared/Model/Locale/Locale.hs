module Shared.Model.Locale.Locale where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Locale = Locale
  { uuid :: U.UUID
  , tenantUuid :: U.UUID
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
