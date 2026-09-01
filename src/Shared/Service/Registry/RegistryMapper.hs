module Shared.Service.Registry.RegistryMapper where

import Data.Time

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Organization.OrganizationCreateDTO
import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import RegistryPublic.Model.Organization.OrganizationSimple
import Shared.Api.Resource.Registry.RegistryCreateDTO
import Shared.Model.Registry.RegistryLocale
import Shared.Model.Registry.RegistryOrganization
import Shared.Model.Registry.RegistryPackage
import Shared.Model.Registry.RegistryTemplate
import Shared.Model.Tenant.Config.WizardTenantConfig

toOrganizationCreate :: TenantConfigOrganization -> RegistryCreateDTO -> OrganizationCreateDTO
toOrganizationCreate tcOrganization reqDto =
  OrganizationCreateDTO
    { organizationId = tcOrganization.organizationId
    , name = tcOrganization.name
    , description = tcOrganization.description
    , email = reqDto.email
    }

toRegistryOrganization :: OrganizationSimple -> UTCTime -> RegistryOrganization
toRegistryOrganization dto now =
  RegistryOrganization
    { organizationId = dto.organizationId
    , name = dto.name
    , logo = dto.logo
    , createdAt = now
    }

toRegistryPackage :: KnowledgeModelPackageSimpleDTO -> UTCTime -> RegistryPackage
toRegistryPackage dto now =
  RegistryPackage
    { organizationId = dto.organizationId
    , kmId = dto.kmId
    , remoteVersion = dto.version
    , createdAt = now
    }

toRegistryTemplate :: DocumentTemplateSimpleDTO -> UTCTime -> RegistryTemplate
toRegistryTemplate dto now =
  RegistryTemplate
    { organizationId = dto.organizationId
    , templateId = dto.templateId
    , remoteVersion = dto.version
    , createdAt = now
    }

toRegistryLocale :: LocaleDTO -> UTCTime -> RegistryLocale
toRegistryLocale dto now =
  RegistryLocale
    { organizationId = dto.organizationId
    , localeId = dto.localeId
    , remoteVersion = dto.version
    , createdAt = now
    }
