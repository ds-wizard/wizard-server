module Shared.Model.Registry.RegistryPackage where

import Data.Time
import GHC.Generics

data RegistryPackage = RegistryPackage
  { id :: String
  , remoteVersion :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
