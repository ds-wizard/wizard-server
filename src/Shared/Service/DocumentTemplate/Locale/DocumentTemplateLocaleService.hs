module Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleCreateDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateLocaleDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList
import Shared.S3.DocumentTemplate.DocumentTemplateLocaleS3
import Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleMapper
import Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleUtil
import Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleValidation
import Shared.Util.Uuid

getLocalesForDocumentTemplate :: WizardRequestContextC s m => U.UUID -> m [DocumentTemplateLocaleList]
getLocalesForDocumentTemplate dtUuid = do
  _ <- findDocumentTemplateByUuid dtUuid
  locales <- findDocumentTemplateLocalesByDocumentTemplateUuid dtUuid
  return . fmap toList $ locales

createLocale :: WizardRequestContextC s m => U.UUID -> DocumentTemplateLocaleCreateDTO -> m DocumentTemplateLocaleList
createLocale dtUuid reqDto =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION
    _ <- findDocumentTemplateByUuid dtUuid
    code <- extractLanguageCode reqDto.poContent
    validateCodeUniqueness dtUuid code
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks (.tenantUuid')
    let locale =
          DocumentTemplateLocale
            { uuid = uuid
            , name = reqDto.name
            , code = code
            , documentTemplateUuid = dtUuid
            , tenantUuid = tenantUuid
            , createdAt = now
            , updatedAt = now
            }
    insertDocumentTemplateLocale locale
    void $ putDocumentTemplateLocale uuid translationPoFileName reqDto.poContent
    return . toList $ locale

getLocaleContent :: WizardRequestContextC s m => U.UUID -> U.UUID -> m BS.ByteString
getLocaleContent dtUuid localeUuid = do
  locale <- findDocumentTemplateLocaleByDocumentTemplateUuidAndUuid dtUuid localeUuid
  retrieveDocumentTemplateLocale locale.uuid translationPoFileName

deleteLocale :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
deleteLocale dtUuid localeUuid =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION
    locale <- findDocumentTemplateLocaleByDocumentTemplateUuidAndUuid dtUuid localeUuid
    void $ deleteDocumentTemplateLocaleByUuid locale.uuid
