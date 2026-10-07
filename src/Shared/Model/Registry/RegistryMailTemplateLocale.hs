module Shared.Model.Registry.RegistryMailTemplateLocale where

import Data.Time
import GHC.Generics

data RegistryMailTemplateLocale = RegistryMailTemplateLocale
  { code :: String
  , name :: String
  , remoteVersion :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
