module Shared.Api.Resource.Locale.Registry.RegistryLocaleDTO where

import GHC.Generics

data RegistryLocaleDTO = RegistryLocaleDTO
  { code :: String
  , name :: String
  , remoteVersion :: String
  , installed :: Bool
  }
  deriving (Show, Eq, Generic)
