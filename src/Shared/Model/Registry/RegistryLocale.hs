module Shared.Model.Registry.RegistryLocale where

import Data.Time
import GHC.Generics

data RegistryLocale = RegistryLocale
  { id :: String
  , remoteVersion :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
