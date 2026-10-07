module Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales where

import qualified Data.ByteString.Char8 as BS

import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoDTO
import Shared.Constant.Tenant
import Shared.Database.Migration.Development.Locale.Data.Locales (toPo)
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Service.MailTemplateLocale.MailTemplateLocaleMapper
import Shared.Util.Date
import Shared.Util.Uuid

mailTemplateLocaleDe :: MailTemplateLocale
mailTemplateLocaleDe =
  MailTemplateLocale
    { uuid = u' "0d3b7a8e-5c41-4f6b-9a2d-3e8f1c7b4a65"
    , tenantUuid = defaultTenantUuid
    , code = "de"
    , name = "Deutsch"
    , potVersion = Just "dsw-mail-templates:5.0.0"
    , enabled = True
    , defaultLocale = False
    , official = True
    , createdAt = dt' 2026 1 21
    , updatedAt = dt' 2026 1 21
    }

mailTemplateLocaleDeDefault :: MailTemplateLocale
mailTemplateLocaleDeDefault = mailTemplateLocaleDe {defaultLocale = True}

mailTemplateLocaleDeEdited :: MailTemplateLocale
mailTemplateLocaleDeEdited =
  mailTemplateLocaleDeDefault
    { name = "Deutsch (neu)"
    , potVersion = Just "dsw-mail-templates:5.1.0"
    , official = False
    }

mailTemplateLocaleDeFromRegistry :: MailTemplateLocale
mailTemplateLocaleDeFromRegistry =
  mailTemplateLocaleDe
    { name = "Deutsch (Registry)"
    , official = False
    }

mailTemplateLocaleCs :: MailTemplateLocale
mailTemplateLocaleCs =
  MailTemplateLocale
    { uuid = u' "c2a6f0de-4b8e-4f0e-8d55-1f7c3b2e9a10"
    , tenantUuid = defaultTenantUuid
    , code = "cs"
    , name = "Čeština"
    , potVersion = Just "dsw-mail-templates:5.0.0"
    , enabled = True
    , defaultLocale = True
    , official = True
    , createdAt = dt' 2026 1 21
    , updatedAt = dt' 2026 1 21
    }

mailTemplateLocaleCsNotDefault :: MailTemplateLocale
mailTemplateLocaleCsNotDefault = mailTemplateLocaleCs {defaultLocale = False}

mailTemplateLocaleNl :: MailTemplateLocale
mailTemplateLocaleNl =
  MailTemplateLocale
    { uuid = u' "7d2c5e8f-1a4b-4c6d-9e3f-2b8a5c1d7e46"
    , tenantUuid = defaultTenantUuid
    , code = "nl"
    , name = "Nederlands"
    , potVersion = Just "fw-client:5.0.0"
    , enabled = False
    , defaultLocale = False
    , official = False
    , createdAt = dt' 2026 1 24
    , updatedAt = dt' 2026 1 24
    }

mailTemplateLocaleDePo :: BS.ByteString
mailTemplateLocaleDePo = toPo "dsw-mail-templates:5.0.0" "de" "Deutsch" [("Settings", "Einstellungen")]

mailTemplateLocaleSv :: MailTemplateLocale
mailTemplateLocaleSv =
  MailTemplateLocale
    { uuid = u' "9a4d6f1c-3e2b-4c8a-b5d7-0e6f2a9c4b13"
    , tenantUuid = defaultTenantUuid
    , code = "sv"
    , name = "Svenska"
    , potVersion = Just "fw-mail-templates:5.0.0"
    , enabled = False
    , defaultLocale = False
    , official = False
    , createdAt = dt' 2026 1 24
    , updatedAt = dt' 2026 1 24
    }

mailTemplateLocaleSvPo :: BS.ByteString
mailTemplateLocaleSvPo = toPo "fw-mail-templates:5.0.0" "sv" "Svenska" [("Projects", "Projekt")]

mailTemplateLocaleDeEditedPo :: BS.ByteString
mailTemplateLocaleDeEditedPo = toPo "dsw-mail-templates:5.1.0" "de" "Deutsch (neu)" [("Settings", "Einstellungen!")]

mailTemplateLocaleNlPo :: BS.ByteString
mailTemplateLocaleNlPo = toPo "fw-client:5.0.0" "nl" "Nederlands" []

mailTemplateLocaleNlCustomKindPo :: BS.ByteString
mailTemplateLocaleNlCustomKindPo = toPo "acme-mail-templates:2026.1" "nl" "Nederlands" [("Projects", "Projecten")]

mailTemplateLocalePoWithoutLanguage :: BS.ByteString
mailTemplateLocalePoWithoutLanguage = "msgid \"\"\nmsgstr \"Project-Id-Version: dsw-mail-templates:5.0.0\\n\"\n"

mailTemplateDefaultPot :: BS.ByteString
mailTemplateDefaultPot = mailTemplatePot "dsw-mail-templates:5.0.0"

mailTemplateCustomPot :: BS.ByteString
mailTemplateCustomPot = mailTemplatePot "acme-mail-templates:2026.1"

mailTemplatePot :: String -> BS.ByteString
mailTemplatePot potVersion =
  BS.pack . unlines $
    [ "msgid \"\""
    , "msgstr \"\""
    , "\"Project-Id-Version: " ++ potVersion ++ "\\n\""
    , "\"Content-Type: text/plain; charset=UTF-8\\n\""
    , ""
    , "msgid \"Settings\""
    , "msgstr \"\""
    ]

mailTemplateLocaleDeDto :: MailTemplateLocaleDTO
mailTemplateLocaleDeDto = toDTO mailTemplateLocaleDe

mailTemplateLocaleDeUploadPreviewDto :: LocaleUploadResultDTO MailTemplateLocaleDTO
mailTemplateLocaleDeUploadPreviewDto =
  PreviewLocaleUploadResultDTO
    { name = mailTemplateLocaleDe.name
    , code = mailTemplateLocaleDe.code
    , potVersion = mailTemplateLocaleDe.potVersion
    , warnings = [toReplaceWarning mailTemplateLocaleDe]
    }

mailTemplateLocaleDeEditedUploadPreviewDto :: LocaleUploadResultDTO MailTemplateLocaleDTO
mailTemplateLocaleDeEditedUploadPreviewDto =
  PreviewLocaleUploadResultDTO
    { name = mailTemplateLocaleDeEdited.name
    , code = mailTemplateLocaleDeEdited.code
    , potVersion = mailTemplateLocaleDeEdited.potVersion
    , warnings = [toReplaceWarning mailTemplateLocaleDe]
    }

mailTemplateLocaleDeFromRegistryUploadPreviewDto :: LocaleUploadResultDTO MailTemplateLocaleDTO
mailTemplateLocaleDeFromRegistryUploadPreviewDto =
  PreviewLocaleUploadResultDTO
    { name = mailTemplateLocaleDeFromRegistry.name
    , code = mailTemplateLocaleDeFromRegistry.code
    , potVersion = mailTemplateLocaleDeFromRegistry.potVersion
    , warnings = [toReplaceWarning mailTemplateLocaleDe]
    }

mailTemplateLocaleNlCustomKindUploadPreviewDto :: LocaleUploadResultDTO MailTemplateLocaleDTO
mailTemplateLocaleNlCustomKindUploadPreviewDto =
  PreviewLocaleUploadResultDTO
    { name = "Nederlands"
    , code = "nl"
    , potVersion = Just "acme-mail-templates:2026.1"
    , warnings = []
    }

mailTemplatePotInfoDto :: MailTemplatePotInfoDTO
mailTemplatePotInfoDto = MailTemplatePotInfoDTO {source = DefaultMailTemplatePotSource, potVersion = Just "dsw-mail-templates:5.0.0"}

mailTemplateCustomPotInfoDto :: MailTemplatePotInfoDTO
mailTemplateCustomPotInfoDto = MailTemplatePotInfoDTO {source = CustomMailTemplatePotSource, potVersion = Just "acme-mail-templates:2026.1"}

mailTemplateNonePotInfoDto :: MailTemplatePotInfoDTO
mailTemplateNonePotInfoDto = MailTemplatePotInfoDTO {source = NoneMailTemplatePotSource, potVersion = Nothing}
