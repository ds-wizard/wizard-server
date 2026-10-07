module Shared.Service.Locale.LocaleMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleSuggestionDTO
import Shared.Model.Locale.Locale
import Shared.Util.Gettext

toDTO :: Locale -> LocaleDTO
toDTO locale =
  LocaleDTO
    { uuid = locale.uuid
    , code = locale.code
    , name = locale.name
    , potVersion = locale.potVersion
    , enabled = locale.enabled
    , defaultLocale = locale.defaultLocale
    , official = locale.official
    , createdAt = locale.createdAt
    , updatedAt = locale.updatedAt
    }

toSuggestionDTO :: Locale -> LocaleSuggestionDTO
toSuggestionDTO locale =
  LocaleSuggestionDTO
    { uuid = locale.uuid
    , code = locale.code
    , name = locale.name
    , defaultLocale = locale.defaultLocale
    }

toReplaceWarning :: Locale -> LocaleUploadWarningDTO
toReplaceWarning locale =
  ReplaceLocaleUploadWarningDTO
    { uuid = locale.uuid
    , name = locale.name
    , code = locale.code
    , official = locale.official
    }

fromChangeDTO :: Locale -> LocaleChangeDTO -> UTCTime -> Locale
fromChangeDTO locale reqDto now = locale {enabled = reqDto.enabled, defaultLocale = reqDto.defaultLocale, updatedAt = now}

fromPoHeader :: U.UUID -> U.UUID -> PoHeaderInfo -> UTCTime -> Locale
fromPoHeader uuid tenantUuid header now =
  Locale
    { uuid = uuid
    , tenantUuid = tenantUuid
    , code = header.code
    , name = header.name
    , potVersion = header.potVersion
    , enabled = False
    , defaultLocale = False
    , official = False
    , createdAt = now
    , updatedAt = now
    }

fromReplace :: Locale -> PoHeaderInfo -> UTCTime -> Locale
fromReplace locale header now = locale {name = header.name, potVersion = header.potVersion, official = False, updatedAt = now}
