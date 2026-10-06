module Shared.Service.Registry.RegistryMapper where

import Data.Time

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import Shared.Model.Registry.RegistryLocale
import Shared.Model.Registry.RegistryPackage
import Shared.Model.Registry.RegistryTemplate

toRegistryPackage :: KnowledgeModelPackageSimpleDTO -> UTCTime -> RegistryPackage
toRegistryPackage dto now =
  RegistryPackage
    { id = dto.id
    , remoteVersion = dto.version
    , createdAt = now
    }

toRegistryTemplate :: DocumentTemplateSimpleDTO -> UTCTime -> RegistryTemplate
toRegistryTemplate dto now =
  RegistryTemplate
    { id = dto.id
    , remoteVersion = dto.version
    , createdAt = now
    }

toRegistryLocale :: LocaleDTO -> UTCTime -> RegistryLocale
toRegistryLocale dto now =
  RegistryLocale
    { id = dto.id
    , remoteVersion = dto.version
    , createdAt = now
    }
