module WizardServer.Service.Locale.LocaleMapper where

import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Api.Resource.Locale.LocaleCreateDTO
import Shared.Model.Locale.Locale
import Shared.Model.Registry.RegistryLocale
import Shared.Service.Version.VersionMapper
import Shared.Util.Reference
import WizardServer.Api.Resource.Locale.LocaleDTO
import WizardServer.Api.Resource.Locale.LocaleDetailDTO
import WizardServer.Model.Locale.LocaleList
import WizardServer.Service.Locale.LocaleUtil

toDTO :: Bool -> LocaleList -> LocaleDTO
toDTO registryEnabled locale =
  LocaleDTO
    { uuid = locale.uuid
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , id = locale.id
    , version = locale.version
    , defaultLocale = locale.defaultLocale
    , enabled = locale.enabled
    , remoteLatestVersion =
        if registryEnabled
          then locale.remoteVersion
          else Nothing
    , createdAt = locale.createdAt
    , updatedAt = locale.updatedAt
    }

toDetailDTO :: Locale -> Bool -> [RegistryLocale] -> [(U.UUID, String)] -> Maybe String -> LocaleDetailDTO
toDetailDTO locale registryEnabled localeRs versionLs registryLink =
  LocaleDetailDTO
    { uuid = locale.uuid
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , id = locale.id
    , version = locale.version
    , defaultLocale = locale.defaultLocale
    , license = locale.license
    , readme = locale.readme
    , recommendedAppVersion = locale.recommendedAppVersion
    , enabled = locale.enabled
    , versions = map toVersionDTO . L.sortBy (\(_, v1) (_, v2) -> compare v2 v1) $ versionLs
    , remoteLatestVersion =
        case (registryEnabled, selectLocaleById locale localeRs) of
          (True, Just localeR) -> Just localeR.remoteVersion
          _ -> Nothing
    , registryLink =
        if registryEnabled
          then registryLink
          else Nothing
    , createdAt = locale.createdAt
    , updatedAt = locale.updatedAt
    }

toLocaleList :: Locale -> LocaleList
toLocaleList locale =
  LocaleList
    { uuid = locale.uuid
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , id = locale.id
    , version = locale.version
    , defaultLocale = locale.defaultLocale
    , enabled = locale.enabled
    , remoteVersion = Nothing
    , createdAt = locale.createdAt
    , updatedAt = locale.updatedAt
    }

fromCreateDTO :: LocaleCreateDTO -> U.UUID -> Bool -> U.UUID -> UTCTime -> Locale
fromCreateDTO reqDto uuid defaultLocale tenantUuid now =
  Locale
    { uuid = uuid
    , name = reqDto.name
    , description = reqDto.description
    , code = reqDto.code
    , id = reqDto.id
    , version = reqDto.version
    , defaultLocale = defaultLocale
    , license = reqDto.license
    , readme = reqDto.readme
    , recommendedAppVersion = reqDto.recommendedAppVersion
    , enabled = False
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: Locale -> LocaleChangeDTO -> UTCTime -> Locale
fromChangeDTO locale reqDto now =
  Locale
    { uuid = locale.uuid
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , id = locale.id
    , version = locale.version
    , defaultLocale = reqDto.defaultLocale
    , license = locale.license
    , readme = locale.readme
    , recommendedAppVersion = locale.recommendedAppVersion
    , enabled = reqDto.enabled
    , tenantUuid = locale.tenantUuid
    , createdAt = locale.createdAt
    , updatedAt = now
    }

buildLocaleUrl :: String -> Locale -> [RegistryLocale] -> Maybe String
buildLocaleUrl clientRegistryUrl locale localeRs =
  case selectLocaleById locale localeRs of
    Just localeR ->
      Just $
        clientRegistryUrl
          ++ "/locales/"
          ++ buildReference localeR.id localeR.remoteVersion
    Nothing -> Nothing
