module Shared.Api.Resource.Settings.SettingsDTO where

import GHC.Generics

data SettingsDTO a = SettingsDTO
  { value :: a
  , overrideAllowed :: Maybe Bool
  , overridden :: Maybe Bool
  }
  deriving (Show, Eq, Generic)
