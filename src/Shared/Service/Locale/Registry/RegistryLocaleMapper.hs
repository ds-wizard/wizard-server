module Shared.Service.Locale.Registry.RegistryLocaleMapper where

import Shared.Api.Resource.Locale.Registry.RegistryLocaleDTO
import Shared.Model.Registry.RegistryLocale
import Shared.Model.Registry.RegistryMailTemplateLocale
import Shared.Util.LocaleCode

toRegistryLocaleDTO :: [String] -> RegistryLocale -> RegistryLocaleDTO
toRegistryLocaleDTO installedCodes locale =
  RegistryLocaleDTO
    { code = locale.code
    , name = locale.name
    , remoteVersion = locale.remoteVersion
    , installed = isInstalled installedCodes locale.code
    }

toRegistryMailTemplateLocaleDTO :: [String] -> RegistryMailTemplateLocale -> RegistryLocaleDTO
toRegistryMailTemplateLocaleDTO installedCodes locale =
  RegistryLocaleDTO
    { code = locale.code
    , name = locale.name
    , remoteVersion = locale.remoteVersion
    , installed = isInstalled installedCodes locale.code
    }

isInstalled :: [String] -> String -> Bool
isInstalled installedCodes code = normalizeLocaleCode code `elem` fmap normalizeLocaleCode installedCodes
