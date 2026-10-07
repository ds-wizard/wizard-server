module Shared.Api.Resource.Locale.Registry.RegistryLocaleJM where

import Data.Aeson

import Shared.Api.Resource.Locale.Registry.RegistryLocaleDTO
import Shared.Util.Aeson

instance FromJSON RegistryLocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RegistryLocaleDTO where
  toJSON = genericToJSON jsonOptions
