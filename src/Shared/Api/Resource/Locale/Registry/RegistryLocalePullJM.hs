module Shared.Api.Resource.Locale.Registry.RegistryLocalePullJM where

import Data.Aeson

import Shared.Api.Resource.Locale.Registry.RegistryLocalePullDTO
import Shared.Util.Aeson

instance FromJSON RegistryLocalePullDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RegistryLocalePullDTO where
  toJSON = genericToJSON jsonOptions
