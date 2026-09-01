module RegistryPublic.Api.Resource.Organization.OrganizationSM where

import Data.Swagger

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryPublic.Model.Organization.OrganizationRole
import Shared.Util.Swagger

instance ToSchema OrganizationRole

instance ToSchema OrganizationDTO where
  declareNamedSchema = toSwagger orgGlobalDTO
