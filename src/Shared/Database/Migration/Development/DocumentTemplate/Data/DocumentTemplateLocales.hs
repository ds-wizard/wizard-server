module Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Time

import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleCreateDTO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList
import Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleMapper
import Shared.Util.Uuid

czechWizardDocumentTemplateLocale :: DocumentTemplateLocale
czechWizardDocumentTemplateLocale =
  DocumentTemplateLocale
    { uuid = u' "40e0f5b4-b0f6-4a1a-9e7e-e8ba5b2a0b62"
    , name = "Czech"
    , code = "cs"
    , documentTemplateUuid = wizardDocumentTemplate.uuid
    , tenantUuid = wizardDocumentTemplate.tenantUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

czechWizardDocumentTemplateLocaleList :: DocumentTemplateLocaleList
czechWizardDocumentTemplateLocaleList = toList czechWizardDocumentTemplateLocale

czechWizardDocumentTemplateLocaleCreate :: DocumentTemplateLocaleCreateDTO
czechWizardDocumentTemplateLocaleCreate =
  DocumentTemplateLocaleCreateDTO
    { name = czechWizardDocumentTemplateLocale.name
    , poContent = czechPoContent
    }

czechPoContent :: BS.ByteString
czechPoContent =
  BS.pack . unlines $
    [ "msgid \"\""
    , "msgstr \"\""
    , "\"Language: cs\\n\""
    , "\"Content-Type: text/plain; charset=UTF-8\\n\""
    , ""
    , "msgid \"Chapter 1\""
    , "msgstr \"Kapitola 1\""
    ]

germanPoContent :: BS.ByteString
germanPoContent =
  BS.pack . unlines $
    [ "msgid \"\""
    , "msgstr \"\""
    , "\"Language: de\\n\""
    , "\"Content-Type: text/plain; charset=UTF-8\\n\""
    , ""
    , "msgid \"Chapter 1\""
    , "msgstr \"Kapitel 1\""
    ]

poContentWithoutLanguage :: BS.ByteString
poContentWithoutLanguage =
  BS.pack . unlines $
    [ "msgid \"\""
    , "msgstr \"\""
    , "\"Content-Type: text/plain; charset=UTF-8\\n\""
    ]

wizardDocumentTemplatePotContent :: BS.ByteString
wizardDocumentTemplatePotContent =
  BS.pack . unlines $
    [ "msgid \"\""
    , "msgstr \"\""
    , "\"Language: en\\n\""
    , "\"Content-Type: text/plain; charset=UTF-8\\n\""
    , ""
    , "msgid \"Chapter 1\""
    , "msgstr \"\""
    ]
