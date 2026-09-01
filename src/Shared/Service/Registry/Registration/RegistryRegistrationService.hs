module Shared.Service.Registry.Registration.RegistryRegistrationService where

import Control.Monad.Reader (liftIO)
import Data.Time

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import Shared.Api.Resource.Registry.RegistryConfirmationDTO
import Shared.Api.Resource.Registry.RegistryCreateDTO
import Shared.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.Registry.Runner
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Service.Registry.RegistryMapper
import Shared.Service.Tenant.Config.ConfigService

signUpToRegistry :: WizardRequestContextC s m => RegistryCreateDTO -> m OrganizationDTO
signUpToRegistry reqDto =
  runInTransaction $ do
    tcOrganization <- findTenantConfigOrganization
    let orgCreateDto = toOrganizationCreate tcOrganization reqDto
    createOrganization orgCreateDto

confirmRegistration :: WizardRequestContextC s m => RegistryConfirmationDTO -> m OrganizationDTO
confirmRegistration reqDto =
  runInTransaction $ do
    org <- confirmOrganizationRegistration reqDto
    tcRegistry <- getCurrentTenantConfigRegistry
    now <- liftIO getCurrentTime
    let updatedRegistry = tcRegistry {enabled = True, token = org.token, updatedAt = now}
    modifyTenantConfigRegistry updatedRegistry
    return org
