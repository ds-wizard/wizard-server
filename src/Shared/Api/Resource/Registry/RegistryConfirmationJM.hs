module Shared.Api.Resource.Registry.RegistryConfirmationJM where

import Data.Aeson

import Shared.Api.Resource.Registry.RegistryConfirmationDTO
import Shared.Util.Aeson

instance FromJSON RegistryConfirmationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RegistryConfirmationDTO where
  toJSON = genericToJSON jsonOptions
