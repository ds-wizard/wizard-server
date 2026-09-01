module Shared.Api.Resource.Registry.RegistryOrganizationJM where

import Data.Aeson

import Shared.Model.Registry.RegistryOrganization
import Shared.Util.Aeson

instance FromJSON RegistryOrganization where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RegistryOrganization where
  toJSON = genericToJSON jsonOptions
