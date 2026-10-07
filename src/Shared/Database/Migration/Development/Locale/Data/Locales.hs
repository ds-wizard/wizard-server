module Shared.Database.Migration.Development.Locale.Data.Locales where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleSuggestionDTO
import Shared.Constant.Tenant
import Shared.Model.Locale.Locale
import Shared.Service.Locale.LocaleMapper
import Shared.Util.Date
import Shared.Util.Gettext
import Shared.Util.Uuid

localeDe :: Locale
localeDe =
  Locale
    { uuid = u' "22fed2bf-9f00-4cf6-a4d4-4a845a0bc273"
    , tenantUuid = defaultTenantUuid
    , code = "de"
    , name = "Deutsch"
    , potVersion = Just "dsw-client:5.0.0"
    , enabled = True
    , defaultLocale = False
    , official = True
    , createdAt = dt' 2026 1 21
    , updatedAt = dt' 2026 1 21
    }

localeCs :: Locale
localeCs =
  Locale
    { uuid = u' "d9894fb9-c6a5-4294-98d6-b46d75684d53"
    , tenantUuid = defaultTenantUuid
    , code = "cs"
    , name = "Čeština"
    , potVersion = Just "dsw-client:4.31.0"
    , enabled = False
    , defaultLocale = False
    , official = False
    , createdAt = dt' 2026 1 22
    , updatedAt = dt' 2026 1 22
    }

differentLocale :: Locale
differentLocale =
  Locale
    { uuid = u' "6881ecac-9e88-4ddd-9df9-d6df166b1c9b"
    , tenantUuid = differentTenantUuid
    , code = "nl"
    , name = "Nederlands"
    , potVersion = Just "dsw-client:5.0.0"
    , enabled = True
    , defaultLocale = True
    , official = False
    , createdAt = dt' 2026 1 21
    , updatedAt = dt' 2026 1 21
    }

localeDeDefault :: Locale
localeDeDefault = localeDe {defaultLocale = True}

localeDeEdited :: Locale
localeDeEdited =
  localeDeDefault
    { name = "Deutsch (Schweiz)"
    , potVersion = Just "dsw-client:5.1.0"
    , official = False
    }

localeDeFromRegistry :: Locale
localeDeFromRegistry =
  localeDe
    { name = "Deutsch (Registry)"
    , official = False
    }

localeCsEnabled :: Locale
localeCsEnabled = localeCs {enabled = True}

localeCsDefault :: Locale
localeCsDefault = localeCs {enabled = True, defaultLocale = True}

localeEn :: Locale
localeEn =
  Locale
    { uuid = u' "5b4b1f0e-2a7c-4e3d-9c1f-8a6d2e4b7c90"
    , tenantUuid = defaultTenantUuid
    , code = "en"
    , name = "English (custom)"
    , potVersion = Just "dsw-client:5.0.0"
    , enabled = True
    , defaultLocale = False
    , official = False
    , createdAt = dt' 2026 1 23
    , updatedAt = dt' 2026 1 23
    }

localeSv :: Locale
localeSv =
  Locale
    { uuid = u' "8c1e4f2a-6b3d-4e7f-9a0c-5d2b8e1f4a37"
    , tenantUuid = defaultTenantUuid
    , code = "sv-SE"
    , name = "Svenska"
    , potVersion = Just "fw-client:5.0.0"
    , enabled = False
    , defaultLocale = False
    , official = False
    , createdAt = dt' 2026 1 24
    , updatedAt = dt' 2026 1 24
    }

localeNlFromRegistry :: Locale
localeNlFromRegistry =
  Locale
    { uuid = u' "3e7a9c1b-4d2f-4a8e-b6c0-7f1d3e5a9b82"
    , tenantUuid = defaultTenantUuid
    , code = "nl"
    , name = "Nederlands"
    , potVersion = Just "dsw-client:5.0.0"
    , enabled = False
    , defaultLocale = False
    , official = False
    , createdAt = dt' 2026 1 24
    , updatedAt = dt' 2026 1 24
    }

localeDePo :: BS.ByteString
localeDePo = toPo "dsw-client:5.0.0" "de" "Deutsch" [("Projects", "Projekte"), ("Settings", "Einstellungen")]

localeCsPo :: BS.ByteString
localeCsPo = toPo "dsw-client:4.31.0" "cs" "Čeština" [("Projects", "Projekty")]

differentLocalePo :: BS.ByteString
differentLocalePo = toPo "dsw-client:5.0.0" "nl" "Nederlands" [("Projects", "Projecten")]

localeDeEditedPo :: BS.ByteString
localeDeEditedPo = toPo "dsw-client:5.1.0" "de" "Deutsch (Schweiz)" [("Projects", "Projekte!")]

localeEnPo :: BS.ByteString
localeEnPo = toPo "dsw-client:5.0.0" "en" "English (custom)" [("Projects", "Plans")]

localeSvPo :: BS.ByteString
localeSvPo = toPo "fw-client:5.0.0" "sv_se" "Svenska" [("Projects", "Projekt")]

localeFrPoWithoutPotVersion :: BS.ByteString
localeFrPoWithoutPotVersion = "msgid \"\"\nmsgstr \"Language: fr\\n\"\n"

localePoWithoutLanguage :: BS.ByteString
localePoWithoutLanguage = "msgid \"\"\nmsgstr \"Project-Id-Version: fw-client:5.0.0\\n\"\n"

localePoUnparsable :: BS.ByteString
localePoUnparsable = "msgstr \"Language: de\\n\"\n"

localePoMailTemplateKind :: BS.ByteString
localePoMailTemplateKind = toPo "fw-mail-templates:5.0.0" "de" "Deutsch" []

localePoInvalidLanguage :: BS.ByteString
localePoInvalidLanguage = toPo "fw-client:5.0.0" "Deutsch" "Deutsch" []

localePoTooLarge :: BS.ByteString
localePoTooLarge = localeSvPo <> "#" <> BS.replicate (5 * 1024 * 1024) ' ' <> "\n"

localeDeJson :: BS.ByteString
localeDeJson = toJedContent localeDePo

localeCsJson :: BS.ByteString
localeCsJson = toJedContent localeCsPo

differentLocaleJson :: BS.ByteString
differentLocaleJson = toJedContent differentLocalePo

localeDeDto :: LocaleDTO
localeDeDto = toDTO localeDe

localeDeSuggestionDto :: LocaleSuggestionDTO
localeDeSuggestionDto = toSuggestionDTO localeDe

localeDeChangeDto :: LocaleChangeDTO
localeDeChangeDto = LocaleChangeDTO {defaultLocale = True, enabled = True}

localeEnabledChangeDto :: LocaleChangeDTO
localeEnabledChangeDto = LocaleChangeDTO {defaultLocale = False, enabled = True}

localeDefaultChangeDto :: LocaleChangeDTO
localeDefaultChangeDto = LocaleChangeDTO {defaultLocale = True, enabled = True}

localeDisabledChangeDto :: LocaleChangeDTO
localeDisabledChangeDto = LocaleChangeDTO {defaultLocale = False, enabled = False}

localeDisabledDefaultChangeDto :: LocaleChangeDTO
localeDisabledDefaultChangeDto = LocaleChangeDTO {defaultLocale = True, enabled = False}

localeDeReplaceWarningDto :: LocaleUploadWarningDTO
localeDeReplaceWarningDto = ReplaceLocaleUploadWarningDTO {uuid = localeDe.uuid, name = localeDe.name, code = localeDe.code, official = localeDe.official}

localeDeUploadPreviewDto :: LocaleUploadResultDTO LocaleDTO
localeDeUploadPreviewDto =
  PreviewLocaleUploadResultDTO
    { name = localeDe.name
    , code = localeDe.code
    , potVersion = localeDe.potVersion
    , warnings = [localeDeReplaceWarningDto]
    }

localeDeEditedUploadPreviewDto :: LocaleUploadResultDTO LocaleDTO
localeDeEditedUploadPreviewDto =
  PreviewLocaleUploadResultDTO
    { name = localeDeEdited.name
    , code = localeDeEdited.code
    , potVersion = localeDeEdited.potVersion
    , warnings = [localeDeReplaceWarningDto]
    }

localeDeFromRegistryUploadPreviewDto :: LocaleUploadResultDTO LocaleDTO
localeDeFromRegistryUploadPreviewDto =
  PreviewLocaleUploadResultDTO
    { name = localeDeFromRegistry.name
    , code = localeDeFromRegistry.code
    , potVersion = localeDeFromRegistry.potVersion
    , warnings = [localeDeReplaceWarningDto]
    }

localeSvUploadPreviewDto :: LocaleUploadResultDTO LocaleDTO
localeSvUploadPreviewDto =
  PreviewLocaleUploadResultDTO
    { name = localeSv.name
    , code = localeSv.code
    , potVersion = localeSv.potVersion
    , warnings = []
    }

localeFrUploadPreviewDto :: LocaleUploadResultDTO LocaleDTO
localeFrUploadPreviewDto =
  PreviewLocaleUploadResultDTO
    { name = "fr"
    , code = "fr"
    , potVersion = Nothing
    , warnings = []
    }

toPo :: String -> String -> String -> [(String, String)] -> BS.ByteString
toPo potVersion code name translations =
  TE.encodeUtf8 . T.pack . unlines $
    [ "msgid \"\""
    , "msgstr \"\""
    , "\"Project-Id-Version: " ++ potVersion ++ "\\n\""
    , "\"Language: " ++ code ++ "\\n\""
    , "\"X-Display-Name: " ++ name ++ "\\n\""
    , "\"Content-Type: text/plain; charset=UTF-8\\n\""
    , "\"Plural-Forms: nplurals=2; plural=(n != 1);\\n\""
    ]
      ++ concatMap (\(msgid, msgstr) -> ["", "msgid \"" ++ msgid ++ "\"", "msgstr \"" ++ msgstr ++ "\""]) translations

toJedContent :: BS.ByteString -> BS.ByteString
toJedContent = BSL.toStrict . encode . toJed . fromRight [] . parsePo
