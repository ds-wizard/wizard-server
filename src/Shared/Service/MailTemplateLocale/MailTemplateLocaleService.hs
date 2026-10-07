module Shared.Service.MailTemplateLocale.MailTemplateLocaleService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Locale.Common.LocaleUploadDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Constant.Gettext
import Shared.Database.DAO.MailTemplateLocale.MailTemplateLocaleDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigMailDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.Locale.Public
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Model.Tenant.Config.TenantConfigMail
import Shared.S3.MailTemplateLocale.MailTemplateLocaleS3
import Shared.Service.Locale.Common.LocaleTemporaryFileService
import Shared.Service.Locale.Common.LocaleUploadService
import Shared.Service.Locale.Common.LocaleValidation
import Shared.Service.MailTemplateLocale.MailTemplateLocaleAuditLog
import Shared.Service.MailTemplateLocale.MailTemplateLocaleMapper
import Shared.Util.Gettext
import Shared.Util.Uuid

getMailTemplateLocales :: WizardRequestContextC s m => m [MailTemplateLocaleDTO]
getMailTemplateLocales = do
  checkPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
  fmap toDTO <$> findMailTemplateLocales

uploadMailTemplateLocale :: WizardRequestContextC s m => LocaleUploadDTO -> m (LocaleUploadResultDTO MailTemplateLocaleDTO)
uploadMailTemplateLocale reqDto = do
  result <-
    runInTransaction $ do
      checkPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
      uploadLocalePo Nothing findMailTemplateLocaleByCode' toReplaceWarning storeMailTemplateLocale reqDto
  removeReplacedMailTemplateLocaleMo result
  return result

modifyMailTemplateLocale :: WizardRequestContextC s m => U.UUID -> LocaleChangeDTO -> m MailTemplateLocaleDTO
modifyMailTemplateLocale uuid reqDto =
  runInTransaction $ do
    checkPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
    locale <- findMailTemplateLocaleByUuid uuid
    validateLocaleChange reqDto locale.defaultLocale
    now <- liftIO getCurrentTime
    let updatedLocale = fromChangeDTO locale reqDto now
    when (updatedLocale.defaultLocale && not locale.defaultLocale) unsetDefaultMailTemplateLocale
    void $ updateMailTemplateLocaleByUuid updatedLocale
    al_updateMailTemplateLocale locale updatedLocale
    return . toDTO $ updatedLocale

deleteMailTemplateLocale :: WizardRequestContextC s m => U.UUID -> m ()
deleteMailTemplateLocale uuid =
  runInTransaction $ do
    checkPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
    locale <- findMailTemplateLocaleByUuid uuid
    void $ deleteMailTemplateLocaleByUuid locale.uuid
    al_deleteMailTemplateLocale locale

getMailTemplateLocalePo :: WizardRequestContextC s m => U.UUID -> m TemporaryFileDTO
getMailTemplateLocalePo uuid =
  runInTransaction $ do
    checkPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
    locale <- findMailTemplateLocaleByUuid uuid
    content <- retrieveMailTemplateLocalePo locale.uuid
    tempFile <- createTemporaryFileWithContent (locale.code ++ ".po") poContentType content
    al_exportMailTemplateLocale locale
    return tempFile

getMailTemplatePot :: WizardRequestContextC s m => m TemporaryFileDTO
getMailTemplatePot =
  runInTransaction $ do
    checkPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
    mPot <- findMailTemplatePot
    case mPot of
      Just (_, pot) -> createTemporaryFileWithContent "mail.pot" potContentType pot
      Nothing -> throwError . NotExistsError $ _ERROR_SERVICE_MAIL_TEMPLATE_LOCALE__POT_NOT_AVAILABLE

getMailTemplatePotInfo :: WizardRequestContextC s m => m MailTemplatePotInfoDTO
getMailTemplatePotInfo = do
  checkPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
  mPot <- findMailTemplatePot
  return . toPotInfoDTO . fmap (fmap potVersionOf) $ mPot

-- --------------------------------
-- PRIVATE
-- --------------------------------
storeMailTemplateLocale :: WizardRequestContextC s m => LocalePo -> Maybe MailTemplateLocale -> m MailTemplateLocaleDTO
storeMailTemplateLocale po Nothing = do
  uuid <- liftIO generateUuid
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let locale = fromPoHeader uuid tenantUuid po.header now
  void $ putMailTemplateLocalePo locale.uuid po.content
  void $ insertMailTemplateLocale locale
  al_createMailTemplateLocale locale
  return . toDTO $ locale
storeMailTemplateLocale po (Just locale) = do
  now <- liftIO getCurrentTime
  let updatedLocale = fromReplace locale po.header now
  void $ putMailTemplateLocalePo updatedLocale.uuid po.content
  void $ updateMailTemplateLocaleByUuid updatedLocale
  al_replaceMailTemplateLocale locale updatedLocale
  return . toDTO $ updatedLocale

removeReplacedMailTemplateLocaleMo :: WizardRequestContextC s m => LocaleUploadResultDTO MailTemplateLocaleDTO -> m ()
removeReplacedMailTemplateLocaleMo (StoredLocaleUploadResultDTO True locale) = removeMailTemplateLocaleMo locale.uuid
removeReplacedMailTemplateLocaleMo _ = return ()

findMailTemplatePot :: WizardRequestContextC s m => m (Maybe (MailTemplatePotSource, BS.ByteString))
findMailTemplatePot = do
  tcMail <- findTenantConfigMail
  mCustomPot <- if tcMail.customTemplates then findCustomMailTemplatePot else return Nothing
  case mCustomPot of
    Just pot -> return . Just $ (CustomMailTemplatePotSource, pot)
    Nothing -> fmap (\pot -> (DefaultMailTemplatePotSource, pot)) <$> findDefaultMailTemplatePot

potVersionOf :: BS.ByteString -> Maybe String
potVersionOf = either (const Nothing) (poPotVersion . poHeaderFields) . parsePo
