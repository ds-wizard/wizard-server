module Shared.Service.MailTemplateLocale.MailTemplateLocaleMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoDTO
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Util.Gettext

toDTO :: MailTemplateLocale -> MailTemplateLocaleDTO
toDTO locale =
  MailTemplateLocaleDTO
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

toReplaceWarning :: MailTemplateLocale -> LocaleUploadWarningDTO
toReplaceWarning locale =
  ReplaceLocaleUploadWarningDTO
    { uuid = locale.uuid
    , name = locale.name
    , code = locale.code
    , official = locale.official
    }

toPotInfoDTO :: Maybe (MailTemplatePotSource, Maybe String) -> MailTemplatePotInfoDTO
toPotInfoDTO (Just (source, potVersion)) = MailTemplatePotInfoDTO {source = source, potVersion = potVersion}
toPotInfoDTO Nothing = MailTemplatePotInfoDTO {source = NoneMailTemplatePotSource, potVersion = Nothing}

fromChangeDTO :: MailTemplateLocale -> LocaleChangeDTO -> UTCTime -> MailTemplateLocale
fromChangeDTO locale reqDto now = locale {enabled = reqDto.enabled, defaultLocale = reqDto.defaultLocale, updatedAt = now}

fromPoHeader :: U.UUID -> U.UUID -> PoHeaderInfo -> UTCTime -> MailTemplateLocale
fromPoHeader uuid tenantUuid header now =
  MailTemplateLocale
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

fromReplace :: MailTemplateLocale -> PoHeaderInfo -> UTCTime -> MailTemplateLocale
fromReplace locale header now = locale {name = header.name, potVersion = header.potVersion, official = False, updatedAt = now}
