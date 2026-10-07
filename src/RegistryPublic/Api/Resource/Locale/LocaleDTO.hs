module RegistryPublic.Api.Resource.Locale.LocaleDTO where

import GHC.Generics

data LocaleDTO = LocaleDTO
  { code :: String
  , name :: String
  , version :: String
  }
  deriving (Show, Eq, Generic)
