module WizardServer.Service.Locale.LocaleService where

import Control.Monad (void, when)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Api.Resource.Locale.LocaleCreateDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Constant.Locale
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.Registry.RegistryLocaleDAO
import Shared.Database.DAO.Registry.RegistryOrganizationDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Locale.Locale
import Shared.Model.Locale.LocaleSimple
import Shared.Model.Locale.LocaleSuggestion
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.Tenant.Tenant
import Shared.S3.Locale.LocaleS3
import Shared.Service.Common
import Shared.Service.Locale.LocaleMapper
import Shared.Service.Locale.LocaleValidation
import Shared.Service.Tenant.Config.ConfigService
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Service.Tenant.TenantHelper
import Shared.Service.Tenant.TenantMapper (toClientUrlBase)
import Shared.Util.Uuid
import WizardServer.Api.Resource.Locale.LocaleDTO
import WizardServer.Api.Resource.Locale.LocaleDetailDTO
import WizardServer.Database.DAO.Locale.LocaleDAO
import WizardServer.Service.Locale.LocaleMapper

getLocalesPage :: WizardRequestContextC s m => Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> m (Page LocaleDTO)
getLocalesPage mOrganizationId mLocaleId mQuery pageable sort = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  checkIfAdminIsDisabled
  locales <- findLocalesPage mOrganizationId mLocaleId mQuery pageable sort
  tcRegistry <- getCurrentTenantConfigRegistry
  return . fmap (toDTO tcRegistry.enabled) $ locales

getLocaleSuggestions :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page LocaleSuggestion)
getLocaleSuggestions mQuery pageable sort = do
  checkIfAdminIsDisabled
  findLocaleSuggestions mQuery pageable sort

createLocale :: WizardRequestContextC s m => LocaleCreateDTO -> m LocaleSimple
createLocale reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    checkIfAdminIsDisabled
    now <- liftIO getCurrentTime
    uuid <- liftIO generateUuid
    tcOrganization <- findTenantConfigOrganization
    let organizationId = tcOrganization.organizationId
    checkLocaleLimit organizationId reqDto.localeId
    validateLocaleCreate reqDto organizationId
    let defaultLocale = False
    let locale = fromCreateDTO reqDto uuid organizationId defaultLocale tcOrganization.tenantUuid now
    insertLocale locale
    putLocale locale.uuid "wizard.json" reqDto.wizardContent
    putLocale locale.uuid "mail.po" reqDto.mailContent
    tcRegistry <- getCurrentTenantConfigRegistry
    return . toSimple $ locale

getLocaleByUuid :: WizardRequestContextC s m => U.UUID -> m LocaleDetailDTO
getLocaleByUuid uuid = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  checkIfAdminIsDisabled
  serverConfig <- asks (.serverConfig')
  locale <- findLocaleByUuid uuid
  versions <- getLocaleVersions locale
  localeRs <- findRegistryLocales
  orgRs <- findRegistryOrganizations
  tcRegistry <- getCurrentTenantConfigRegistry
  return $ toDetailDTO locale tcRegistry.enabled localeRs orgRs versions (buildLocaleUrl serverConfig.registry.clientUrl locale localeRs)

getLocaleContentForCurrentUser :: WizardRequestContextC s m => Maybe String -> m BS.ByteString
getLocaleContentForCurrentUser mClientUrl = do
  checkIfAdminIsDisabled
  tenant <- maybe getCurrentTenant (findTenantByClientUrl . toClientUrlBase) mClientUrl
  mUser <- asks (.currentUser')
  locale <-
    case mUser of
      Just user ->
        case user.locale of
          Just localeUuid -> findLocaleSuggestionBy [tenantQueryUuid tenant.uuid, ("uuid", U.toString localeUuid)]
          Nothing -> findLocaleSuggestionBy [tenantQueryUuid tenant.uuid, ("default_locale", show True)]
      Nothing -> findLocaleSuggestionBy [tenantQueryUuid tenant.uuid, ("default_locale", show True)]
  if not (locale.organizationId == defaultLocaleOrganizationId && locale.localeId == defaultLocaleLocaleId && locale.version == defaultLocaleVersion)
    then retrieveLocaleWithTenant tenant.uuid locale.uuid "wizard.json"
    else return "{}"

modifyLocale :: WizardRequestContextC s m => U.UUID -> LocaleChangeDTO -> m LocaleDTO
modifyLocale uuid reqDto = do
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    checkIfAdminIsDisabled
    now <- liftIO getCurrentTime
    locale <- findLocaleByUuid uuid
    validateLocaleChange reqDto locale
    let updatedLocale = fromChangeDTO locale reqDto now
    when (updatedLocale.defaultLocale && not locale.defaultLocale) unsetDefaultLocale
    when (updatedLocale.enabled && not locale.enabled) (unsetEnabledLocale updatedLocale.code)
    updateLocaleByUuid updatedLocale
    tcRegistry <- getCurrentTenantConfigRegistry
    return . toDTO tcRegistry.enabled $ toLocaleList updatedLocale

deleteLocalesByQueryParams :: WizardRequestContextC s m => [(String, String)] -> m ()
deleteLocalesByQueryParams queryParams =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    checkIfAdminIsDisabled
    locales <- findLocalesFiltered queryParams
    traverse_ validateLocaleDeletion locales
    traverse_ (deleteLocaleByUuid . (.uuid)) locales

deleteLocale :: WizardRequestContextC s m => U.UUID -> m ()
deleteLocale uuid =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    checkIfAdminIsDisabled
    locale <- findLocaleByUuid uuid
    validateLocaleDeletion locale
    void $ deleteLocaleByUuid locale.uuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
getLocaleVersions :: WizardRequestContextC s m => Locale -> m [(U.UUID, String)]
getLocaleVersions locale = do
  allLocales <- findLocalesByOrganizationIdAndLocaleId locale.organizationId locale.localeId
  return . fmap (\l -> (l.uuid, l.version)) $ allLocales

checkIfAdminIsDisabled :: WizardRequestContextC s m => m ()
checkIfAdminIsDisabled =
  checkIfServerFeatureIsEnabled "Locale Endpoints" (\s -> not s.admin.enabled)
