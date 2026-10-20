module WizardServer.Service.Mcp.Tool.TenantTools where

import Control.Monad.Except (throwError)
import Data.Aeson
import Data.Proxy
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.Mcp.Tenant.McpTenantAdminDTO
import Shared.Api.Resource.Mcp.Tenant.McpTenantAdminJM ()
import Shared.Api.Resource.Tenant.TenantChangeDTO
import Shared.Api.Resource.Tenant.TenantCreateDTO
import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Api.Resource.Tenant.TenantDetailDTO
import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Api.Resource.Tenant.Usage.WizardUsageJM ()
import Shared.Api.Resource.Tenant.WizardTenantJM ()
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.Tenant.Limit.TenantLimitBundleChange
import Shared.Model.Tenant.Tenant
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Mcp.Tool.TenantToolCommon
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Service.Tenant.TenantMapper (toChangeDTO)
import Shared.Service.Tenant.TenantService
import Shared.Service.Tenant.Usage.WizardUsageService
import Shared.Util.JsonSchema
import WizardServer.Api.Resource.Tenant.TenantChangeJM ()
import WizardServer.Api.Resource.Tenant.TenantDetailJM ()

tenantTools :: WizardRequestContextC s m => [McpTool m]
tenantTools = [listTenantsTool, getTenantTool, saveTenantTool, deleteTenantTool, updateTenantLimitsTool]

listTenantsTool :: WizardRequestContextC s m => McpTool m
listTenantsTool =
  McpTool
    { name = "list_tenants"
    , title = "List tenants"
    , description = "List the tenants of the instance, filtered by name or tenantId (q), by state and by whether they are enabled"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "tenants" (jsonSchema (Proxy :: Proxy TenantDTO))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = tenantReadPermissions
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(mStates, mEnabled, (mQuery, pageable)) ->
        toJSON <$> getTenantsPage mQuery mStates mEnabled pageable [Sort "name" Ascending]
    }
  where
    arguments =
      (,,)
        <$> optionalArgument "states" "Only the tenants in one of these states"
        <*> optionalArgument "enabled" "Only the enabled (true) or disabled (false) tenants"
        <*> searchArguments "Search in the names and tenantIds of the tenants"
        :: Arguments (Maybe [TenantState], Maybe Bool, (Maybe String, Pageable))

getTenantTool :: WizardRequestContextC s m => McpTool m
getTenantTool =
  McpTool
    { name = "get_tenant"
    , title = "Get tenant"
    , description = "Get a tenant with its users who manage users and its usage against its limits"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just . withProperties [("usage", describedSchema "The usage, shaped as update_tenant_limits returns it" (typeSchema "object"))] $ jsonSchema (Proxy :: Proxy TenantDetailDTO)
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = tenantReadPermissions
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(uuid, usage) -> withUsage usage (getTenantByUuid uuid) (getUsage uuid)
    }
  where
    arguments =
      (,) <$> tenantUuidArgument <*> usageArgument "Also count what the tenant uses against its limits, true by default"
        :: Arguments (U.UUID, Bool)

saveTenantTool :: WizardRequestContextC s m => McpTool m
saveTenantTool =
  McpTool
    { name = "save_tenant"
    , title = "Save tenant"
    , description =
        "Create a tenant or change its tenantId (the subdomain of its URL) and name; only the fields you give change. "
          ++ "A new tenant needs tenantId, name and admin, its first admin, who gets an account with a random password and sets their own through the forgotten-password link. "
          ++ uuidDescription "a tenant"
          ++ ". "
          ++ expectedUpdatedAtDescription "tenant"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy TenantDetailDTO)
    , annotations = writeAnnotations
    , continuable = False
    , permissions = tenantWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(mUuid, mExpectedUpdatedAt, mAdmin, changes) ->
        toJSON <$> saveTenant mUuid mExpectedUpdatedAt mAdmin changes
    }
  where
    arguments =
      (,,,)
        <$> uuidArgument tenantEntityName
        <*> expectedUpdatedAtArgument
        <*> optionalArgument "admin" "The first admin of a new tenant"
        <*> changesArguments (jsonSchema (Proxy :: Proxy TenantChangeDTO))
        :: Arguments (Maybe U.UUID, Maybe UTCTime, Maybe McpTenantAdminDTO, Object)

saveTenant :: WizardRequestContextC s m => Maybe U.UUID -> Maybe UTCTime -> Maybe McpTenantAdminDTO -> Object -> m TenantDetailDTO
saveTenant mUuid mExpectedUpdatedAt mAdmin changes =
  runInTransaction $ do
    tenant <- saveByUuid findTenantByUuid' (createTenantFromChanges mAdmin changes) (updateTenantFromChanges mExpectedUpdatedAt changes) mUuid
    getTenantByUuid tenant.uuid

createTenantFromChanges :: WizardRequestContextC s m => Maybe McpTenantAdminDTO -> Object -> U.UUID -> m Tenant
createTenantFromChanges mAdmin changes uuid = do
  requireChanges tenantEntityName ["tenantId", "name"] changes
  admin <- maybe (throwError . UserError $ _ERROR_SERVICE_MCP__MISSING_ARGUMENTS tenantEntityName ["admin"]) return mAdmin
  reqDto <- mergeChanges changes (TenantChangeDTO "" "")
  _ <- createTenantByAdminWithUuid uuid (TenantCreateDTO reqDto.tenantId reqDto.name admin.firstName admin.lastName admin.email "")
  findTenantByUuid uuid

updateTenantFromChanges :: WizardRequestContextC s m => Maybe UTCTime -> Object -> Tenant -> m Tenant
updateTenantFromChanges mExpectedUpdatedAt changes tenant = do
  let current = toChangeDTO tenant
  changed <- mergeChangesSince tenantEntityName mExpectedUpdatedAt tenant.updatedAt changes current
  modifyIfChanged current changed (return tenant) (modifyTenant tenant.uuid)

deleteTenantTool :: WizardRequestContextC s m => McpTool m
deleteTenantTool =
  McpTool
    { name = "delete_tenant"
    , title = "Delete tenant"
    , description =
        "Delete a tenant with all its users, projects, knowledge models, templates and files. "
          ++ "Deleting cannot be undone. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments tenantUuidArgument)
    , outputSchema = Just $ confirmationOutputSchema (objectSchema [] [])
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = tenantWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "delete_tenant" tenantUuidArgument prepareTenantDeletion
    }

prepareTenantDeletion :: WizardRequestContextC s m => U.UUID -> m (McpConfirmation m)
prepareTenantDeletion uuid = do
  tenant <- getTenantByUuid uuid
  return
    McpConfirmation
      { summary = "Deletes the tenant " ++ tenant.name ++ " (" ++ tenant.tenantId ++ ", " ++ tenant.clientUrl ++ ") with all its data; it cannot be undone"
      , revision = iso8601Show tenant.updatedAt
      , perform = object [] <$ deleteTenant uuid
      }

updateTenantLimitsTool :: WizardRequestContextC s m => McpTool m
updateTenantLimitsTool =
  McpTool
    { name = "update_tenant_limits"
    , title = "Update tenant limits"
    , description = "Change the limits of a tenant; only the limits you give change (storage in bytes). It returns the usage against them"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy WizardUsageDTO)
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = tenantWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(uuid, changes) -> toJSON <$> updateTenantLimits uuid changes
    }
  where
    arguments =
      (,) <$> tenantUuidArgument <*> changesArguments (jsonSchema (Proxy :: Proxy TenantLimitBundleChange))
        :: Arguments (U.UUID, Object)

updateTenantLimits :: WizardRequestContextC s m => U.UUID -> Object -> m WizardUsageDTO
updateTenantLimits uuid changes = do
  _ <- getTenantByUuid uuid
  limits <- mergeChanges changes =<< findTenantLimits uuid
  modifyLimitBundle uuid limits
