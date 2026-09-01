module Shared.S3.KnowledgeModel.KnowledgeModelLocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "knowledge-model-locales"

translationPoFileName = "translation.po"

translationJsonFileName = "translation.json"

retrieveKnowledgeModelLocale :: WizardRequestContextC s m => U.UUID -> String -> m BS.ByteString
retrieveKnowledgeModelLocale localeUuid fileName = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName])

putKnowledgeModelLocale :: WizardRequestContextC s m => U.UUID -> String -> BS.ByteString -> m String
putKnowledgeModelLocale localeUuid fileName = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName]) Nothing Nothing

removeKnowledgeModelLocales :: WizardRequestContextC s m => m ()
removeKnowledgeModelLocales = createRemoveObjectFn folderName

removeKnowledgeModelLocale :: WizardRequestContextC s m => U.UUID -> m ()
removeKnowledgeModelLocale localeUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString localeUuid])
