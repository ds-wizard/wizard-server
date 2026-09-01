module RegistryPublic.Database.Migration.Development.Organization.Data.Organizations where

import Data.Maybe (fromJust)
import Data.Time

import RegistryPublic.Api.Resource.Organization.OrganizationCreateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationStateDTO
import RegistryPublic.Model.Organization.Organization
import RegistryPublic.Model.Organization.OrganizationRole
import RegistryPublic.Model.Organization.OrganizationSimple
import RegistryPublic.Service.Organization.OrganizationMapper

orgGlobal :: Organization
orgGlobal =
  Organization
    { organizationId = "global"
    , name = "Organization"
    , description = "Some description of Organization"
    , email = "organization@example.com"
    , oRole = AdminRole
    , token = "GlobalToken"
    , active = True
    , logo = Just orgLogo
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

orgGlobalSimple :: OrganizationSimple
orgGlobalSimple =
  OrganizationSimple
    { name = "Organization"
    , organizationId = "global"
    , logo = Just orgLogo
    }

orgGlobalDTO :: OrganizationDTO
orgGlobalDTO = toDTO orgGlobal

orgGlobalCreate :: OrganizationCreateDTO
orgGlobalCreate =
  OrganizationCreateDTO
    { organizationId = orgGlobal.organizationId
    , name = orgGlobal.name
    , description = orgGlobal.description
    , email = orgGlobal.email
    }

orgGlobalEdited :: Organization
orgGlobalEdited =
  Organization
    { organizationId = orgGlobal.organizationId
    , name = "EDITED: Organization"
    , description = "EDITED: Some description of Organization"
    , email = "edited-organization@example.com"
    , oRole = orgGlobal.oRole
    , token = orgGlobal.token
    , active = orgGlobal.active
    , logo = orgGlobal.logo
    , createdAt = orgGlobal.createdAt
    , updatedAt = orgGlobal.updatedAt
    }

orgNetherlands :: Organization
orgNetherlands =
  Organization
    { organizationId = "org.nl"
    , name = "Organization Netherlands"
    , description = "Some description of Organization Netherlands"
    , email = "netherlands@example.com"
    , oRole = UserRole
    , token = "NetherlandsToken"
    , active = True
    , logo = Just orgLogo
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

orgNetherlandsSimple :: OrganizationSimple
orgNetherlandsSimple =
  OrganizationSimple
    { name = "Organization Netherlands"
    , organizationId = "org.nl"
    , logo = Just orgLogo
    }

orgStateDto :: OrganizationStateDTO
orgStateDto = OrganizationStateDTO {active = True}

orgLogo :: String
orgLogo =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+P+/HgAFhAJ/wlseKgAAAABJRU5ErkJggg=="
