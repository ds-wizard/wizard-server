module RegistryPublic.Service.Organization.OrganizationMapper where

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Model.Organization.Organization

toDTO :: Organization -> OrganizationDTO
toDTO organization =
  OrganizationDTO
    { organizationId = organization.organizationId
    , name = organization.name
    , description = organization.description
    , email = organization.email
    , oRole = organization.oRole
    , token = organization.token
    , logo = organization.logo
    , active = organization.active
    , createdAt = organization.createdAt
    , updatedAt = organization.updatedAt
    }
