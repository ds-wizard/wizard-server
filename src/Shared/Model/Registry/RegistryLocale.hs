module Shared.Model.Registry.RegistryLocale where

import Data.Time
import GHC.Generics

data RegistryLocale = RegistryLocale
  { code :: String
  , name :: String
  , remoteVersion :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
