module Shared.Database.Migration.Development.Registry.Data.RegistryOrganizations where

import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryPublic.Model.Organization.OrganizationSimple
import Shared.Model.Registry.RegistryOrganization
import Shared.Util.Date

globalRegistryOrganization :: RegistryOrganization
globalRegistryOrganization =
  RegistryOrganization
    { organizationId = orgGlobalSimple.organizationId
    , name = orgGlobalSimple.name
    , logo = orgGlobalSimple.logo
    , createdAt = dt' 2018 1 21
    }

nlRegistryOrganization :: RegistryOrganization
nlRegistryOrganization =
  RegistryOrganization
    { organizationId = orgNetherlandsSimple.organizationId
    , name = orgNetherlandsSimple.name
    , logo = orgNetherlandsSimple.logo
    , createdAt = dt' 2018 1 21
    }
