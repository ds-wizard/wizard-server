module Shared.S3.DocumentTemplate.DocumentTemplateLocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "document-template-locales"

translationPoFileName = "translation.po"

retrieveDocumentTemplateLocale :: WizardRequestContextC s m => U.UUID -> String -> m BS.ByteString
retrieveDocumentTemplateLocale localeUuid fileName = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName])

putDocumentTemplateLocale :: WizardRequestContextC s m => U.UUID -> String -> BS.ByteString -> m String
putDocumentTemplateLocale localeUuid fileName = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName]) Nothing Nothing

removeDocumentTemplateLocales :: WizardRequestContextC s m => m ()
removeDocumentTemplateLocales = createRemoveObjectFn folderName

removeDocumentTemplateLocale :: WizardRequestContextC s m => U.UUID -> m ()
removeDocumentTemplateLocale localeUuid = createRemoveObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, translationPoFileName])
