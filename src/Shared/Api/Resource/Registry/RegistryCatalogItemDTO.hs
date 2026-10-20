module Shared.Api.Resource.Registry.RegistryCatalogItemDTO where

import GHC.Generics

data RegistryCatalogItemDTO = RegistryCatalogItemDTO
  { kind :: String
  , id :: String
  , name :: String
  , description :: String
  , latestVersion :: String
  }
  deriving (Show, Eq, Generic)
