module Shared.Service.Tenant.Config.ConfigService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Tenant.Config.WizardTenantConfigChangeDTO
import Shared.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigProjectDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigRegistryDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Common.SensitiveData
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.Tenant.Config.WizardTenantConfigEM ()
import Shared.Service.Tenant.Config.ConfigMapper
import Shared.Service.Tenant.Config.ConfigValidation
import Shared.Service.Tenant.Config.WizardConfigMapper

getCurrentTenantConfigDto :: WizardRequestContextC s m => m TenantConfig
getCurrentTenantConfigDto = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  tcOrganization <- findTenantConfigOrganization
  tcAuthentication <- getCurrentTenantConfigAuthentication
  tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
  tcDashboardAndLoginScreen <- findTenantConfigDashboardAndLoginScreen
  tcLookAndFeel <- findTenantConfigLookAndFeel
  tcRegistry <- getCurrentTenantConfigRegistry
  tcProject <- getCurrentTenantConfigProject
  tcSubmission <- findTenantConfigSubmission
  tcFeatures <- findTenantConfigFeatures
  tcOwl <- findTenantConfigOwl
  return $ toTenantConfig tcOrganization tcAuthentication tcPrivacyAndSupport tcDashboardAndLoginScreen tcLookAndFeel tcRegistry tcProject tcSubmission tcFeatures tcOwl

modifyTenantConfigDto :: WizardRequestContextC s m => TenantConfigChangeDTO -> m TenantConfig
modifyTenantConfigDto reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    validateTenantConfig reqDto
    now <- liftIO getCurrentTime
    -- Organization
    tcOrganization <- findTenantConfigOrganization
    let tcOrganizationUpdated = fromOrganizationChangeDTO reqDto.organization tcOrganization.tenantUuid tcOrganization.createdAt now
    updateTenantConfigOrganization tcOrganizationUpdated
    -- Authentication
    tcAuthentication <- getCurrentTenantConfigAuthentication
    let tcAuthenticationUpdated = fromAuthenticationChangeDTO reqDto.authentication tcAuthentication.tenantUuid tcAuthentication.createdAt now
    modifyTenantConfigAuthentication tcAuthenticationUpdated
    -- PrivacyAndSupport
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    let tcPrivacyAndSupportUpdated = fromPrivacyAndSupportChangeDTO reqDto.privacyAndSupport tcPrivacyAndSupport.tenantUuid tcPrivacyAndSupport.createdAt now
    updateTenantConfigPrivacyAndSupport tcPrivacyAndSupportUpdated
    -- DashboardAndLoginScreen
    tcDashboardAndLoginScreen <- findTenantConfigLookAndFeel
    let tcDashboardAndLoginScreenUpdated = fromDashboardAndLoginScreenChangeDTO reqDto.dashboardAndLoginScreen tcDashboardAndLoginScreen.tenantUuid tcDashboardAndLoginScreen.createdAt now
    updateTenantConfigDashboardAndLoginScreen tcDashboardAndLoginScreenUpdated
    -- LookAndFeel
    tcLookAndFeel <- findTenantConfigLookAndFeel
    let tcLookAndFeelUpdated = fromLookAndFeelChangeDTO reqDto.lookAndFeel tcLookAndFeel.tenantUuid tcLookAndFeel.createdAt now
    updateTenantConfigLookAndFeel tcLookAndFeelUpdated
    -- Registry
    tcRegistry <- getCurrentTenantConfigRegistry
    let tcRegistryUpdated = fromRegistryChangeDTO reqDto.registry tcRegistry.tenantUuid tcRegistry.createdAt now
    modifyTenantConfigRegistry tcRegistryUpdated
    -- Project
    tcProject <- getCurrentTenantConfigProject
    let tcProjectUpdated = fromProjectChangeDTO reqDto.project tcProject.tenantUuid tcProject.createdAt now
    modifyTenantConfigProject tcProjectUpdated
    -- Submission
    tcSubmission <- findTenantConfigSubmission
    let tcSubmissionUpdated = fromSubmissionChangeDTO reqDto.submission tcSubmission.tenantUuid tcSubmission.createdAt now
    updateTenantConfigSubmission tcSubmissionUpdated
    -- Features
    tcFeatures <- findTenantConfigFeatures
    let tcFeaturesUpdated = fromFeaturesChangeDTO reqDto.features tcFeatures tcFeatures.tenantUuid tcFeatures.createdAt tcFeatures.updatedAt
    updateTenantConfigFeatures tcFeaturesUpdated
    -- Owl
    tcOwl <- findTenantConfigOwl
    return $ toTenantConfig tcOrganizationUpdated tcAuthenticationUpdated tcPrivacyAndSupportUpdated tcDashboardAndLoginScreenUpdated tcLookAndFeelUpdated tcRegistryUpdated tcProjectUpdated tcSubmissionUpdated tcFeaturesUpdated tcOwl

getCurrentTenantConfigAuthentication :: WizardRequestContextC s m => m TenantConfigAuthentication
getCurrentTenantConfigAuthentication = do
  serverConfig <- asks (.serverConfig')
  encryptedTcAuthentication <- findTenantConfigAuthentication
  return $ process serverConfig.general.secret encryptedTcAuthentication

getTenantConfigAuthenticationByUuid :: WizardRequestContextC s m => U.UUID -> m TenantConfigAuthentication
getTenantConfigAuthenticationByUuid tenantUuid = do
  serverConfig <- asks (.serverConfig')
  encryptedTcAuthentication <- findTenantConfigAuthenticationByUuid tenantUuid
  return $ process serverConfig.general.secret encryptedTcAuthentication

modifyTenantConfigAuthentication :: WizardRequestContextC s m => TenantConfigAuthentication -> m TenantConfigAuthentication
modifyTenantConfigAuthentication tcAuthentication =
  runInTransaction $ do
    serverConfig <- asks (.serverConfig')
    let encryptedUpdatedTcAuthentication = process serverConfig.general.secret tcAuthentication
    updateTenantConfigAuthentication encryptedUpdatedTcAuthentication
    return tcAuthentication

getCurrentTenantConfigRegistry :: WizardRequestContextC s m => m TenantConfigRegistry
getCurrentTenantConfigRegistry = do
  serverConfig <- asks (.serverConfig')
  encryptedTcRegistry <- findTenantConfigRegistry
  return $ process serverConfig.general.secret encryptedTcRegistry

getTenantConfigRegistryByUuid :: WizardRequestContextC s m => U.UUID -> m TenantConfigRegistry
getTenantConfigRegistryByUuid tenantUuid = do
  serverConfig <- asks (.serverConfig')
  encryptedTcRegistry <- findTenantConfigRegistryByUuid tenantUuid
  return $ process serverConfig.general.secret encryptedTcRegistry

modifyTenantConfigRegistry :: WizardRequestContextC s m => TenantConfigRegistry -> m TenantConfigRegistry
modifyTenantConfigRegistry tcRegistry =
  runInTransaction $ do
    serverConfig <- asks (.serverConfig')
    let encryptedUpdatedTcRegistry = process serverConfig.general.secret tcRegistry
    updateTenantConfigRegistry encryptedUpdatedTcRegistry
    return tcRegistry

getCurrentTenantConfigProject :: WizardRequestContextC s m => m TenantConfigProject
getCurrentTenantConfigProject = do
  serverConfig <- asks (.serverConfig')
  encryptedTcProject <- findTenantConfigProject
  return $ process serverConfig.general.secret encryptedTcProject

getTenantConfigProjectByUuid :: WizardRequestContextC s m => U.UUID -> m TenantConfigProject
getTenantConfigProjectByUuid tenantUuid = do
  serverConfig <- asks (.serverConfig')
  encryptedTcProject <- findTenantConfigProjectByUuid tenantUuid
  return $ process serverConfig.general.secret encryptedTcProject

modifyTenantConfigProject :: WizardRequestContextC s m => TenantConfigProject -> m TenantConfigProject
modifyTenantConfigProject tcProject =
  runInTransaction $ do
    serverConfig <- asks (.serverConfig')
    let encryptedUpdatedTcProject = process serverConfig.general.secret tcProject
    updateTenantConfigProject encryptedUpdatedTcProject
    return tcProject
