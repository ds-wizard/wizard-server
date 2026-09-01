module RegistryPublic.Api.Resource.Organization.OrganizationJM where

import Data.Aeson

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationRoleJM ()
import Shared.Util.Aeson

instance ToJSON OrganizationDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OrganizationDTO where
  parseJSON = genericParseJSON jsonOptions
