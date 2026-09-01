module RegistryPublic.Api.Resource.Organization.OrganizationSimpleJM where

import Data.Aeson

import RegistryPublic.Model.Organization.OrganizationSimple
import Shared.Util.Aeson

instance FromJSON OrganizationSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OrganizationSimple where
  toJSON = genericToJSON jsonOptions
