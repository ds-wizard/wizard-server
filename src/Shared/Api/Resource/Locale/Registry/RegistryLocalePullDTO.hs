module Shared.Api.Resource.Locale.Registry.RegistryLocalePullDTO where

import GHC.Generics

newtype RegistryLocalePullDTO = RegistryLocalePullDTO
  { confirmed :: Bool
  }
  deriving (Show, Eq, Generic)
