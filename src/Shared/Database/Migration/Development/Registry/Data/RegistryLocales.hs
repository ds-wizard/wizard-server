module Shared.Database.Migration.Development.Registry.Data.RegistryLocales where

import qualified Data.ByteString.Char8 as BS

import Shared.Api.Resource.Locale.Registry.RegistryLocaleDTO
import Shared.Api.Resource.Locale.Registry.RegistryLocalePullDTO
import Shared.Database.Migration.Development.Locale.Data.Locales (toPo)
import Shared.Model.Registry.RegistryLocale
import Shared.Model.Registry.RegistryMailTemplateLocale
import Shared.Util.Date

deRegistryLocale :: RegistryLocale
deRegistryLocale =
  RegistryLocale
    { code = "de"
    , name = "Deutsch"
    , remoteVersion = "5.0.0"
    , createdAt = dt' 2026 1 21
    }

nlRegistryLocale :: RegistryLocale
nlRegistryLocale =
  RegistryLocale
    { code = "nl"
    , name = "Nederlands"
    , remoteVersion = "5.0.0"
    , createdAt = dt' 2026 1 21
    }

deRegistryMailTemplateLocale :: RegistryMailTemplateLocale
deRegistryMailTemplateLocale =
  RegistryMailTemplateLocale
    { code = "de"
    , name = "Deutsch"
    , remoteVersion = "5.0.0"
    , createdAt = dt' 2026 1 21
    }

deRegistryLocaleDto :: RegistryLocaleDTO
deRegistryLocaleDto = RegistryLocaleDTO {code = "de", name = "Deutsch", remoteVersion = "5.0.0", installed = True}

nlRegistryLocaleDto :: RegistryLocaleDTO
nlRegistryLocaleDto = RegistryLocaleDTO {code = "nl", name = "Nederlands", remoteVersion = "5.0.0", installed = False}

deRegistryMailTemplateLocaleDto :: RegistryLocaleDTO
deRegistryMailTemplateLocaleDto = RegistryLocaleDTO {code = "de", name = "Deutsch", remoteVersion = "5.0.0", installed = True}

deRegistryLocalePo :: BS.ByteString
deRegistryLocalePo = toPo "dsw-client:5.0.0" "de" "Deutsch (Registry)" [("Projects", "Projekte")]

nlRegistryLocalePo :: BS.ByteString
nlRegistryLocalePo = toPo "dsw-client:5.0.0" "nl" "Nederlands" [("Projects", "Projecten")]

deRegistryMailTemplateLocalePo :: BS.ByteString
deRegistryMailTemplateLocalePo = toPo "dsw-mail-templates:5.0.0" "de" "Deutsch (Registry)" [("Settings", "Einstellungen")]

registryLocalePullDto :: RegistryLocalePullDTO
registryLocalePullDto = RegistryLocalePullDTO {confirmed = True}

registryLocalePreviewDto :: RegistryLocalePullDTO
registryLocalePreviewDto = RegistryLocalePullDTO {confirmed = False}
