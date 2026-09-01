module Shared.Api.Resource.Registry.RegistryCreateDTO where

import GHC.Generics

data RegistryCreateDTO = RegistryCreateDTO
  { email :: String
  }
  deriving (Show, Eq, Generic)
