module Shared.Service.Locale.Registry.RegistryLocaleService where

import Shared.Api.Resource.Locale.Common.LocaleUploadDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.Registry.RegistryLocaleDTO
import Shared.Api.Resource.Locale.Registry.RegistryLocalePullDTO
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.Registry.RegistryLocaleDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.Registry.Runner
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Locale.Locale
import Shared.Model.Registry.RegistryLocale
import Shared.Service.Locale.Common.LocaleUploadService
import Shared.Service.Locale.LocaleMapper (toReplaceWarning)
import Shared.Service.Locale.LocaleService (storeLocale)
import Shared.Service.Locale.Registry.RegistryLocaleMapper

getRegistryLocales :: WizardRequestContextC s m => m [RegistryLocaleDTO]
getRegistryLocales = do
  checkPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
  installedCodes <- fmap (.code) <$> findLocales
  fmap (toRegistryLocaleDTO installedCodes) <$> findRegistryLocales

pullRegistryLocale :: WizardRequestContextC s m => String -> RegistryLocalePullDTO -> m (LocaleUploadResultDTO LocaleDTO)
pullRegistryLocale code reqDto =
  runInTransaction $ do
    checkPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
    registryLocale <- findRegistryLocaleByCode code
    content <- retrieveRegistryLocalePo registryLocale.code registryLocale.remoteVersion
    let uploadDto = LocaleUploadDTO {content = content, confirmed = reqDto.confirmed}
    uploadLocalePo (Just uiLocaleKinds) findLocaleByCode' toReplaceWarning storeLocale uploadDto
