module Shared.Service.MailTemplateLocale.Registry.RegistryMailTemplateLocaleService where

import Shared.Api.Resource.Locale.Common.LocaleUploadDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.Registry.RegistryLocaleDTO
import Shared.Api.Resource.Locale.Registry.RegistryLocalePullDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Database.DAO.MailTemplateLocale.MailTemplateLocaleDAO
import Shared.Database.DAO.Registry.RegistryMailTemplateLocaleDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.Registry.Runner
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Model.Registry.RegistryMailTemplateLocale
import Shared.Service.Locale.Common.LocaleUploadService
import Shared.Service.Locale.Registry.RegistryLocaleMapper
import Shared.Service.MailTemplateLocale.MailTemplateLocaleMapper (toReplaceWarning)
import Shared.Service.MailTemplateLocale.MailTemplateLocaleService (removeReplacedMailTemplateLocaleMo, storeMailTemplateLocale)

getRegistryMailTemplateLocales :: WizardRequestContextC s m => m [RegistryLocaleDTO]
getRegistryMailTemplateLocales = do
  checkPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
  installedCodes <- fmap (.code) <$> findMailTemplateLocales
  fmap (toRegistryMailTemplateLocaleDTO installedCodes) <$> findRegistryMailTemplateLocales

pullRegistryMailTemplateLocale :: WizardRequestContextC s m => String -> RegistryLocalePullDTO -> m (LocaleUploadResultDTO MailTemplateLocaleDTO)
pullRegistryMailTemplateLocale code reqDto = do
  result <-
    runInTransaction $ do
      checkPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
      registryLocale <- findRegistryMailTemplateLocaleByCode code
      content <- retrieveRegistryMailTemplateLocalePo registryLocale.code registryLocale.remoteVersion
      let uploadDto = LocaleUploadDTO {content = content, confirmed = reqDto.confirmed}
      uploadLocalePo Nothing findMailTemplateLocaleByCode' toReplaceWarning storeMailTemplateLocale uploadDto
  removeReplacedMailTemplateLocaleMo result
  return result
