module Shared.Integration.Http.Registry.Runner where

import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types.Status (statusCode)
import Servant

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import Shared.Integration.Http.Common.HttpClient
import Shared.Integration.Http.Common.ServantClient
import Shared.Integration.Http.Registry.RequestMapper
import Shared.Integration.Http.Registry.ResponseMapper
import Shared.Localization.Messages.Internal
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Model.Settings.Settings
import Shared.Model.Statistics.InstanceStatistics
import Shared.Service.Settings.OrganizationSettingsService
import Shared.Util.Logger

retrievePackages :: WizardRequestContextC s m => InstanceStatistics -> m [KnowledgeModelPackageSimpleDTO]
retrievePackages iStat = do
  tcRegistry <- getCurrentSettingsRegistry
  if tcRegistry.enabled
    then
      catchError
        ( do
            let request = toRetrievePackagesRequest tcRegistry iStat
            res <- runRegistryClient request
            return . getResponse $ res
        )
        (\_ -> return [])
    else return []

retrieveKnowledgeModelBundleById :: WizardRequestContextC s m => String -> m BSL.ByteString
retrieveKnowledgeModelBundleById pkgId = do
  serverConfig <- asks (.serverConfig')
  tcRegistry <- getCurrentSettingsRegistry
  if tcRegistry.enabled
    then
      runRegistryRequest $
        runRequest
          (toRetrieveKnowledgeModelBundleByIdRequest serverConfig.registry tcRegistry pkgId)
          toRetrieveKnowledgeModelBundleByIdResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

retrieveDocumentTemplates :: WizardRequestContextC s m => m [DocumentTemplateSimpleDTO]
retrieveDocumentTemplates = do
  tcRegistry <- getCurrentSettingsRegistry
  if tcRegistry.enabled
    then
      catchError
        ( do
            let request = toRetrieveDocumentTemplatesRequest tcRegistry
            res <- runRegistryClient request
            return . getResponse $ res
        )
        (\_ -> return [])
    else return []

retrieveDocumentTemplateBundleByCoordinate :: WizardRequestContextC s m => Coordinate -> m BSL.ByteString
retrieveDocumentTemplateBundleByCoordinate coordinate = do
  serverConfig <- asks (.serverConfig')
  tcRegistry <- getCurrentSettingsRegistry
  if tcRegistry.enabled
    then
      runRegistryRequest $
        runRequest
          (toRetrieveDocumentTemplateBundleByCoordinateRequest serverConfig.registry tcRegistry coordinate)
          toRetrieveDocumentTemplateBundleByCoordinateResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

retrieveLocales :: WizardRequestContextC s m => m [LocaleDTO]
retrieveLocales = do
  buildInfoConfig <- asks (.buildInfoConfig')
  tcRegistry <- getCurrentSettingsRegistry
  if tcRegistry.enabled
    then
      catchError
        ( do
            let request = toRetrieveLocaleRequest buildInfoConfig.releaseVersion tcRegistry
            res <- runRegistryClient request
            return . getResponse $ res
        )
        (\_ -> return [])
    else return []

retrieveLocaleBundleByCoordinate :: WizardRequestContextC s m => Coordinate -> m BSL.ByteString
retrieveLocaleBundleByCoordinate coordinate = do
  serverConfig <- asks (.serverConfig')
  tcRegistry <- getCurrentSettingsRegistry
  if tcRegistry.enabled
    then
      runRegistryRequest $
        runRequest
          (toRetrieveLocaleBundleByCoordinateRequest serverConfig.registry tcRegistry coordinate)
          toRetrieveLocaleBundleByCoordinateResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

uploadKnowledgeModelBundle :: WizardRequestContextC s m => KnowledgeModelBundle -> m KnowledgeModelBundle
uploadKnowledgeModelBundle reqDto = do
  tcRegistry <- getCurrentSettingsRegistry
  let request = toUploadKnowledgeModelBundleRequest tcRegistry reqDto
  res <- runRegistryRequest $ runRegistryClient request
  return . getResponse $ res

uploadDocumentTemplateBundle :: WizardRequestContextC s m => BSL.ByteString -> m BSL.ByteString
uploadDocumentTemplateBundle bundle = do
  serverConfig <- asks (.serverConfig')
  tcRegistry <- getCurrentSettingsRegistry
  runRegistryRequest $
    runRequest
      (toUploadDocumentTemplateBundleRequest serverConfig.registry tcRegistry bundle)
      toUploadDocumentTemplateBundleResponse

uploadLocaleBundle :: WizardRequestContextC s m => BSL.ByteString -> m BSL.ByteString
uploadLocaleBundle bundle = do
  serverConfig <- asks (.serverConfig')
  tcRegistry <- getCurrentSettingsRegistry
  runRegistryRequest $
    runRequest
      (toUploadLocaleBundleRequest serverConfig.registry tcRegistry bundle)
      toUploadLocaleBundleResponse

-- --------------------------------
-- PRIVATE
-- --------------------------------
runRegistryRequest :: WizardRequestContextC s m => m a -> m a
runRegistryRequest = flip catchError handleError
  where
    handleError error
      | isAuthFailure error = do
          logWarnI _CMP_INTEGRATION "Registry rejected the API key"
          throwError . UserError $ _ERROR_SERVICE_REGISTRY__API_KEY_REJECTED
      | otherwise = throwError error
    isAuthFailure (HttpClientError status _) = statusCode status `elem` authStatusCodes
    isAuthFailure (GeneralServerError message) = message `elem` fmap (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR . ("statusCode: " ++) . show) authStatusCodes
    isAuthFailure _ = False
    authStatusCodes = [401, 403] :: [Int]
