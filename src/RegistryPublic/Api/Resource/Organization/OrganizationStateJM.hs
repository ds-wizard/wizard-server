module RegistryPublic.Api.Resource.Organization.OrganizationStateJM where

import Data.Aeson

import RegistryPublic.Api.Resource.Organization.OrganizationStateDTO
import Shared.Util.Aeson

instance ToJSON OrganizationStateDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OrganizationStateDTO where
  parseJSON = genericParseJSON jsonOptions
