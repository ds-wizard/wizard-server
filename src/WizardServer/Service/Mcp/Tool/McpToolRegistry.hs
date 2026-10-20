module WizardServer.Service.Mcp.Tool.McpToolRegistry where

import Control.Monad (void)

import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailJM ()
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Api.Resource.Tenant.Usage.WizardUsageJM ()
import Shared.Api.Resource.User.UserPasswordDTO
import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Database.DAO.User.UserOpenIdIdentityDAO
import Shared.Model.Cache.ServerCache
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpSettingsSection
import Shared.Model.Mcp.McpTool
import Shared.Model.User.UserOpenIdIdentity
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Service.Dev.DevOperationDefinitions
import Shared.Service.Dev.WizardDevOperationService (checkIfAdminIsDisabled)
import Shared.Service.Mcp.Tool.McpExecutableServices
import qualified Shared.Service.Mcp.Tool.McpToolRegistry as Shared
import Shared.Service.Mcp.Tool.OpenIdClientTools
import Shared.Service.Mcp.Tool.OperationTools
import Shared.Service.Mcp.Tool.SettingsSecretMapper
import Shared.Service.Mcp.Tool.SettingsTools
import Shared.Service.PersistentCommand.WizardPersistentCommandService
import Shared.Service.Settings.OrganizationSettingsService
import Shared.Service.Tenant.Usage.WizardUsageService
import Shared.Service.User.UserService (changeUserPasswordByAdminOrHash, deleteUser, resetUserPassword)
import Shared.Service.UserToken.ApiKey.ApiKeyService
import Shared.Service.UserToken.UserTokenService
import WizardServer.Service.Mcp.Tool.RegistryLocaleTools
import WizardServer.Service.Mcp.Tool.TenantTools
import WizardServer.Service.Mcp.Tool.UserTools
import WizardServer.Service.OpenId.Client.Definition.OpenIdClientDefinitionService

mcpTools :: WizardRequestContextC s m => [McpTool m]
mcpTools =
  Shared.mcpTools getUsageForCurrentTenant [registrySection]
    ++ userTools services
    ++ Shared.accountTools services
    ++ openIdClientTools services
    ++ registryLocaleTools
    ++ tenantTools
    ++ operationTools operationServices
  where
    registrySection = (maskedSettingsSection "registry" maskRegistry unmaskRegistry getSettingsRegistry modifySettingsRegistry) {sensitive = True}

services :: WizardRequestContextC s m => McpExecutableServices m
services =
  McpExecutableServices
    { findUserLoginProviders = fmap (fmap (.providerUuid)) . findUserOpenIdIdentitiesByUserUuid
    , setUserPassword = \uuid password -> changeUserPasswordByAdminOrHash uuid (UserPasswordDTO password) Nothing
    , sendPasswordLink = resetUserPassword . UserEmailLinkDTO ForgottenPasswordUserEmailLinkType
    , createApiKey = createApiKey
    , deleteToken = deleteTokenByUuid
    , deleteUser = deleteUser
    , createOpenIdClient = \uuid -> void . createOpenIdClientDefinitionWithUuid uuid
    , modifyOpenIdClient = \uuid -> void . modifyOpenIdClientDefinition uuid
    , deleteOpenIdClient = deleteOpenIdClientDefinition
    }

operationServices :: WizardRequestContextC s m => McpOperationServices m PersistentCommandDetailDTO
operationServices =
  McpOperationServices
    { devSections = sections <$ checkIfAdminIsDisabled
    , getPersistentCommandsPage = getPersistentCommandsPage
    , getPersistentCommandById = getPersistentCommandById
    , modifyPersistentCommand = modifyPersistentCommand
    , runPersistentCommandById = runPersistentCommandById
    }
