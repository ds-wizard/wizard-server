module Shared.Service.Locale.Bundle.LocaleBundleService where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Api.Resource.LocaleBundle.LocaleBundleDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.Registry.Runner
import Shared.Localization.Messages.Internal
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Model.Locale.Locale
import Shared.Model.Locale.LocaleSimple
import Shared.S3.Locale.LocaleS3
import Shared.Service.Locale.Bundle.LocaleBundleAudit
import Shared.Service.Locale.Bundle.LocaleBundleMapper
import Shared.Service.Locale.LocaleMapper
import Shared.Service.Locale.LocaleValidation
import qualified Shared.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import Shared.Service.TemporaryFile.TemporaryFileService
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Util.String
import Shared.Util.Uuid

getTemporaryFileWithBundle :: WizardRequestContextC s m => U.UUID -> m TemporaryFileDTO
getTemporaryFileWithBundle uuid =
  runInTransaction $ do
    (coordinate, bundle) <- exportBundle uuid
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile (f' "%s.zip" [show coordinate]) "application/octet-stream" mCurrentUserUuid bundle
    return $ TemporaryFileMapper.toDTO url "application/octet-stream"

exportBundle :: WizardRequestContextC s m => U.UUID -> m (Coordinate, BSL.ByteString)
exportBundle uuid =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    locale <- findLocaleByUuid uuid
    wizardTranslation <- retrieveLocale locale.uuid "wizard.json"
    mailTranslation <- retrieveLocale locale.uuid "mail.po"
    return (createCoordinate locale, toLocaleArchive locale wizardTranslation mailTranslation)

pullBundleFromRegistry :: WizardRequestContextC s m => Coordinate -> m LocaleSimple
pullBundleFromRegistry coordinate =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    lb <- catchError (retrieveLocaleBundleByCoordinate coordinate) handleError
    importBundle lb True
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_LB__PULL_NON_EXISTING_LOCALE (show coordinate)
        else throwError error

importBundle :: WizardRequestContextC s m => BSL.ByteString -> Bool -> m LocaleSimple
importBundle contentS fromRegistry =
  case fromLocaleArchive contentS of
    Right (bundle, wizardTranslation, mailTranslation) -> do
      checkLocaleLimit bundle.organizationId bundle.localeId
      validateLocaleIdUniqueness (createCoordinate bundle)
      uuid <- liftIO generateUuid
      tenantUuid <- asks (.tenantUuid')
      let locale = fromLocaleBundle bundle uuid tenantUuid
      putLocale locale.uuid "wizard.json" wizardTranslation
      putLocale locale.uuid "mail.po" mailTranslation
      insertLocale locale
      if fromRegistry
        then auditLocaleBundlePullFromRegistry (createCoordinate locale)
        else auditLocaleBundleImportFromFile (createCoordinate locale)
      return . toSimple $ locale
    Left error -> throwError error
