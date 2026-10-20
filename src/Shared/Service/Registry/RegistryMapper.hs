module Shared.Service.Registry.RegistryMapper where

import Data.Time

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.Registry.RegistryCatalogItemDTO
import Shared.Model.Registry.RegistryLocale
import Shared.Model.Registry.RegistryMailTemplateLocale
import Shared.Model.Registry.RegistryPackage
import Shared.Model.Registry.RegistryTemplate

toRegistryPackage :: KnowledgeModelPackageSimpleDTO -> UTCTime -> RegistryPackage
toRegistryPackage dto now =
  RegistryPackage
    { id = dto.id
    , remoteVersion = dto.version
    , createdAt = now
    , remoteName = dto.name
    , remoteDescription = dto.description
    }

toRegistryTemplate :: DocumentTemplateSimpleDTO -> UTCTime -> RegistryTemplate
toRegistryTemplate dto now =
  RegistryTemplate
    { id = dto.id
    , remoteVersion = dto.version
    , createdAt = now
    , remoteName = dto.name
    , remoteDescription = dto.description
    }

toRegistryLocale :: LocaleDTO -> UTCTime -> RegistryLocale
toRegistryLocale dto now =
  RegistryLocale
    { code = dto.code
    , name = dto.name
    , remoteVersion = dto.version
    , createdAt = now
    }

toRegistryMailTemplateLocale :: LocaleDTO -> UTCTime -> RegistryMailTemplateLocale
toRegistryMailTemplateLocale dto now =
  RegistryMailTemplateLocale
    { code = dto.code
    , name = dto.name
    , remoteVersion = dto.version
    , createdAt = now
    }

toPackageCatalogItem :: RegistryPackage -> RegistryCatalogItemDTO
toPackageCatalogItem package =
  RegistryCatalogItemDTO
    { kind = "knowledgeModel"
    , id = package.id
    , name = package.remoteName
    , description = package.remoteDescription
    , latestVersion = package.remoteVersion
    }

toTemplateCatalogItem :: RegistryTemplate -> RegistryCatalogItemDTO
toTemplateCatalogItem template =
  RegistryCatalogItemDTO
    { kind = "documentTemplate"
    , id = template.id
    , name = template.remoteName
    , description = template.remoteDescription
    , latestVersion = template.remoteVersion
    }
