module Shared.Model.Registry.RegistryTemplate where

import Data.Time
import GHC.Generics

data RegistryTemplate = RegistryTemplate
  { id :: String
  , remoteVersion :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
