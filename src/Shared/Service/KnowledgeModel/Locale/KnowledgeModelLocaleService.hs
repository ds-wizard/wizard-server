module Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleService where

import Control.Monad (void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (Value, decodeStrict)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Shared.S3.KnowledgeModel.KnowledgeModelLocaleS3
import Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleMapper
import Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleUtil
import Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleValidation
import Shared.Util.Uuid

getLocalesForPackage :: WizardRequestContextC s m => U.UUID -> m [KnowledgeModelLocaleList]
getLocalesForPackage pkgUuid = do
  _ <- findPackageByUuid pkgUuid
  locales <- findKnowledgeModelLocalesByPackageUuid pkgUuid
  return . fmap toList $ locales

createLocale :: WizardRequestContextC s m => U.UUID -> KnowledgeModelLocaleCreateDTO -> m KnowledgeModelLocaleList
createLocale pkgUuid reqDto =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
    _ <- findPackageByUuid pkgUuid
    code <- extractLanguageCode reqDto.poContent
    validateJsonContent reqDto.jsonContent
    validateCodeUniqueness pkgUuid code
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks (.tenantUuid')
    let locale =
          KnowledgeModelLocale
            { uuid = uuid
            , name = reqDto.name
            , code = code
            , knowledgeModelPackageUuid = pkgUuid
            , tenantUuid = tenantUuid
            , createdAt = now
            , updatedAt = now
            }
    insertKnowledgeModelLocale locale
    putKnowledgeModelLocale uuid translationPoFileName reqDto.poContent
    putKnowledgeModelLocale uuid translationJsonFileName reqDto.jsonContent
    return . toList $ locale

getLocaleContent :: WizardRequestContextC s m => U.UUID -> U.UUID -> m BS.ByteString
getLocaleContent pkgUuid localeUuid = do
  locale <- findKnowledgeModelLocaleByPackageUuidAndUuid pkgUuid localeUuid
  retrieveKnowledgeModelLocale locale.uuid translationPoFileName

deleteLocale :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
deleteLocale pkgUuid localeUuid =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
    locale <- findKnowledgeModelLocaleByPackageUuidAndUuid pkgUuid localeUuid
    void $ deleteProjectCachesByKnowledgeModelLocale pkgUuid locale.code
    void $ deleteKnowledgeModelLocaleByUuid locale.uuid

findLocaleJson :: WizardRequestContextC s m => U.UUID -> Maybe String -> m (Maybe Value)
findLocaleJson _ Nothing = return Nothing
findLocaleJson pkgUuid (Just code) =
  catchError
    ( do
        mLocale <- findKnowledgeModelLocaleByPackageUuidAndCode' pkgUuid code
        case mLocale of
          Just locale -> do
            content <- retrieveKnowledgeModelLocale locale.uuid translationJsonFileName
            return . decodeStrict $ content
          Nothing -> return Nothing
    )
    (\_ -> return Nothing)

getReusableLocalesForEditor :: WizardRequestContextC s m => U.UUID -> m [KnowledgeModelLocaleList]
getReusableLocalesForEditor editorUuid = do
  checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
  editor <- findKnowledgeModelEditorByUuid editorUuid
  case editor.previousPackageUuid of
    Just previousPackageUuid -> do
      locales <- findKnowledgeModelLocalesByPackageUuid previousPackageUuid
      return . fmap toList $ locales
    Nothing -> return []

copyLocalesForPublishedPackage :: WizardRequestContextC s m => Maybe [U.UUID] -> Maybe U.UUID -> U.UUID -> m ()
copyLocalesForPublishedPackage Nothing _ _ = return ()
copyLocalesForPublishedPackage (Just []) _ _ = return ()
copyLocalesForPublishedPackage (Just localeUuids) mPreviousPackageUuid targetPkgUuid =
  case mPreviousPackageUuid of
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__KM_LOCALE_NOT_REUSABLE
    Just previousPackageUuid -> do
      reusableLocales <- findKnowledgeModelLocalesByPackageUuid previousPackageUuid
      let selectedLocales = filter (\l -> l.uuid `elem` localeUuids) reusableLocales
      when
        (length selectedLocales /= length localeUuids)
        (throwError . UserError $ _ERROR_VALIDATION__KM_LOCALE_NOT_REUSABLE)
      copyLocales targetPkgUuid selectedLocales

copyLocales :: WizardRequestContextC s m => U.UUID -> [KnowledgeModelLocale] -> m ()
copyLocales targetPkgUuid locales = do
  now <- liftIO getCurrentTime
  mapM_ (copyLocale now) locales
  where
    copyLocale now locale = do
      newUuid <- liftIO generateUuid
      let newLocale =
            locale
              { uuid = newUuid
              , knowledgeModelPackageUuid = targetPkgUuid
              , createdAt = now
              , updatedAt = now
              }
      insertKnowledgeModelLocale newLocale
      poContent <- retrieveKnowledgeModelLocale locale.uuid translationPoFileName
      jsonContent <- retrieveKnowledgeModelLocale locale.uuid translationJsonFileName
      putKnowledgeModelLocale newUuid translationPoFileName poContent
      void $ putKnowledgeModelLocale newUuid translationJsonFileName jsonContent
