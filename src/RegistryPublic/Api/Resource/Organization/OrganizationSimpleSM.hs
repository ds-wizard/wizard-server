module RegistryPublic.Api.Resource.Organization.OrganizationSimpleSM where

import Data.Swagger

import RegistryPublic.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryPublic.Model.Organization.OrganizationSimple
import Shared.Util.Swagger

instance ToSchema OrganizationSimple where
  declareNamedSchema = toSwagger orgGlobalSimple
