module Shared.Integration.Http.Registry.Runner where

import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy as BSL
import Servant

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Organization.OrganizationCreateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationStateJM ()
import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import RegistryPublic.Model.Organization.OrganizationSimple
import Shared.Api.Resource.Registry.RegistryConfirmationDTO
import Shared.Integration.Http.Common.HttpClient
import Shared.Integration.Http.Common.ServantClient
import Shared.Integration.Http.Registry.RequestMapper
import Shared.Integration.Http.Registry.ResponseMapper
import Shared.Localization.Messages.Public
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Model.Statistics.InstanceStatistics
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Service.Tenant.Config.ConfigService
import Shared.Service.Tenant.TenantHelper

retrieveOrganizations :: WizardRequestContextC s m => m [OrganizationSimple]
retrieveOrganizations = do
  tcRegistry <- getCurrentTenantConfigRegistry
  if tcRegistry.enabled
    then do
      let request = toRetrieveOrganizationsRequest
      res <- runRegistryClient request
      return . getResponse $ res
    else return []

createOrganization :: WizardRequestContextC s m => OrganizationCreateDTO -> m OrganizationDTO
createOrganization reqDto = do
  serverConfig <- asks (.serverConfig')
  clientUrl <- getClientUrl
  let request = toCreateOrganizationRequest serverConfig reqDto clientUrl
  res <- runRegistryClient request
  return . getResponse $ res

confirmOrganizationRegistration :: WizardRequestContextC s m => RegistryConfirmationDTO -> m OrganizationDTO
confirmOrganizationRegistration reqDto = do
  let request = toConfirmOrganizationRegistrationRequest reqDto
  res <- runRegistryClient request
  return . getResponse $ res

retrievePackages :: WizardRequestContextC s m => InstanceStatistics -> m [KnowledgeModelPackageSimpleDTO]
retrievePackages iStat = do
  tcRegistry <- getCurrentTenantConfigRegistry
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
  tcRegistry <- getCurrentTenantConfigRegistry
  if tcRegistry.enabled
    then
      runRequest
        (toRetrieveKnowledgeModelBundleByIdRequest serverConfig.registry tcRegistry pkgId)
        toRetrieveKnowledgeModelBundleByIdResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

retrieveDocumentTemplates :: WizardRequestContextC s m => m [DocumentTemplateSimpleDTO]
retrieveDocumentTemplates = do
  tcRegistry <- getCurrentTenantConfigRegistry
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
  tcRegistry <- getCurrentTenantConfigRegistry
  if tcRegistry.enabled
    then
      runRequest
        (toRetrieveDocumentTemplateBundleByCoordinateRequest serverConfig.registry tcRegistry coordinate)
        toRetrieveDocumentTemplateBundleByCoordinateResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

retrieveLocales :: WizardRequestContextC s m => m [LocaleDTO]
retrieveLocales = do
  buildInfoConfig <- asks (.buildInfoConfig')
  tcRegistry <- getCurrentTenantConfigRegistry
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
  tcRegistry <- getCurrentTenantConfigRegistry
  if tcRegistry.enabled
    then
      runRequest
        (toRetrieveLocaleBundleByCoordinateRequest serverConfig.registry tcRegistry coordinate)
        toRetrieveLocaleBundleByCoordinateResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

uploadKnowledgeModelBundle :: WizardRequestContextC s m => KnowledgeModelBundle -> m KnowledgeModelBundle
uploadKnowledgeModelBundle reqDto = do
  tcRegistry <- getCurrentTenantConfigRegistry
  let request = toUploadKnowledgeModelBundleRequest tcRegistry reqDto
  res <- runRegistryClient request
  return . getResponse $ res

uploadDocumentTemplateBundle :: WizardRequestContextC s m => BSL.ByteString -> m BSL.ByteString
uploadDocumentTemplateBundle bundle = do
  serverConfig <- asks (.serverConfig')
  tcRegistry <- getCurrentTenantConfigRegistry
  runRequest
    (toUploadDocumentTemplateBundleRequest serverConfig.registry tcRegistry bundle)
    toUploadDocumentTemplateBundleResponse

uploadLocaleBundle :: WizardRequestContextC s m => BSL.ByteString -> m BSL.ByteString
uploadLocaleBundle bundle = do
  serverConfig <- asks (.serverConfig')
  tcRegistry <- getCurrentTenantConfigRegistry
  runRequest
    (toUploadLocaleBundleRequest serverConfig.registry tcRegistry bundle)
    toUploadLocaleBundleResponse
