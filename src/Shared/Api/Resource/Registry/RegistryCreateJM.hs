module Shared.Api.Resource.Registry.RegistryCreateJM where

import Data.Aeson

import Shared.Api.Resource.Registry.RegistryCreateDTO
import Shared.Util.Aeson

instance FromJSON RegistryCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RegistryCreateDTO where
  toJSON = genericToJSON jsonOptions
