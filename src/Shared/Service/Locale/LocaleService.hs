module Shared.Service.Locale.LocaleService where

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Locale.Common.LocaleUploadDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleSuggestionDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Constant.Gettext
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Locale.Locale
import Shared.Model.Tenant.Tenant
import Shared.S3.Locale.LocaleS3
import Shared.Service.Locale.Common.LocaleTemporaryFileService
import Shared.Service.Locale.Common.LocaleUploadService
import Shared.Service.Locale.Common.LocaleValidation
import Shared.Service.Locale.LocaleAuditLog
import Shared.Service.Locale.LocaleMapper
import Shared.Service.Tenant.TenantHelper
import Shared.Util.Gettext
import Shared.Util.LocaleCode
import Shared.Util.Uuid

getLocales :: WizardRequestContextC s m => m [LocaleDTO]
getLocales = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  fmap toDTO <$> findLocales

getLocaleSuggestions :: WizardRequestContextC s m => m [LocaleSuggestionDTO]
getLocaleSuggestions = fmap toSuggestionDTO <$> findEnabledLocales

getLocaleContentForCurrentUser :: WizardRequestContextC s m => Maybe String -> m BS.ByteString
getLocaleContentForCurrentUser mClientUrl = do
  tenant <- maybe getCurrentTenant (findTenantByClientUrl . toClientUrlBase) mClientUrl
  mUser <- asks (.currentUser')
  locales <- findEnabledLocalesWithTenant tenant.uuid
  case selectContentLocale (mUser >>= (.locale)) locales of
    Just locale -> retrieveLocaleJsonWithTenant tenant.uuid locale.uuid
    Nothing -> return "{}"

uploadLocale :: WizardRequestContextC s m => LocaleUploadDTO -> m (LocaleUploadResultDTO LocaleDTO)
uploadLocale reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    uploadLocalePo (Just uiLocaleKinds) findLocaleByCode' toReplaceWarning storeLocale reqDto

modifyLocale :: WizardRequestContextC s m => U.UUID -> LocaleChangeDTO -> m LocaleDTO
modifyLocale uuid reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    locale <- findLocaleByUuid uuid
    validateLocaleChange reqDto locale.defaultLocale
    now <- liftIO getCurrentTime
    let updatedLocale = fromChangeDTO locale reqDto now
    when (updatedLocale.defaultLocale && not locale.defaultLocale) unsetDefaultLocale
    void $ updateLocaleByUuid updatedLocale
    al_updateLocale locale updatedLocale
    return . toDTO $ updatedLocale

deleteLocale :: WizardRequestContextC s m => U.UUID -> m ()
deleteLocale uuid =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    locale <- findLocaleByUuid uuid
    void $ deleteLocaleByUuid locale.uuid
    al_deleteLocale locale

modifyLocaleFlags :: WizardRequestContextC s m => U.UUID -> Maybe Bool -> Maybe Bool -> m LocaleDTO
modifyLocaleFlags uuid mEnabled mDefaultLocale = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  locale <- findLocaleByUuid uuid
  modifyLocale uuid (toLocaleChange locale.enabled locale.defaultLocale mEnabled mDefaultLocale)

getLocalePoContent :: WizardRequestContextC s m => U.UUID -> m BS.ByteString
getLocalePoContent uuid =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    locale <- findLocaleByUuid uuid
    content <- retrieveLocalePo locale.uuid
    al_exportLocale locale
    return content

getLocalePo :: WizardRequestContextC s m => U.UUID -> m TemporaryFileDTO
getLocalePo uuid =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    locale <- findLocaleByUuid uuid
    content <- retrieveLocalePo locale.uuid
    tempFile <- createTemporaryFileWithContent (locale.code ++ ".po") poContentType content
    al_exportLocale locale
    return tempFile

determineLocale :: WizardRequestContextC s m => Maybe String -> m (Maybe U.UUID)
determineLocale Nothing = return Nothing
determineLocale (Just acceptLanguage) = do
  locales <- findEnabledLocales
  return . matchAcceptLanguage acceptLanguage . fmap (\l -> (l.code, l.uuid)) $ locales

-- --------------------------------
-- PRIVATE
-- --------------------------------
storeLocale :: WizardRequestContextC s m => LocalePo -> Maybe Locale -> m LocaleDTO
storeLocale po Nothing = do
  uuid <- liftIO generateUuid
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let locale = fromPoHeader uuid tenantUuid po.header now
  putLocaleFiles locale po
  void $ insertLocale locale
  al_createLocale locale
  return . toDTO $ locale
storeLocale po (Just locale) = do
  now <- liftIO getCurrentTime
  let updatedLocale = fromReplace locale po.header now
  putLocaleFiles updatedLocale po
  void $ updateLocaleByUuid updatedLocale
  al_replaceLocale locale updatedLocale
  return . toDTO $ updatedLocale

putLocaleFiles :: WizardRequestContextC s m => Locale -> LocalePo -> m ()
putLocaleFiles locale po = do
  void $ putLocalePo locale.uuid po.content
  void $ putLocaleJson locale.uuid (BSL.toStrict . encode . toJed $ po.entries)

selectContentLocale :: Maybe U.UUID -> [Locale] -> Maybe Locale
selectContentLocale mUserLocaleUuid locales =
  (mUserLocaleUuid >>= \uuid -> L.find (\l -> l.uuid == uuid) locales)
    <|> L.find (.defaultLocale) locales
    <|> L.find (\l -> l.code == "en") locales
