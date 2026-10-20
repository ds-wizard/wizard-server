module Shared.Service.Mcp.Tool.TenantToolCommon where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.UUID as U

import Shared.Model.User.RolePermission
import Shared.Service.Mcp.Tool.McpToolCommon

withUsage :: (Monad m, ToJSON tenant, ToJSON usage) => Bool -> m tenant -> m usage -> m Value
withUsage False getTenant _ = toJSON <$> getTenant
withUsage True getTenant getTenantUsage = do
  tenant <- getTenant
  usage <- getTenantUsage
  return $ case toJSON tenant of
    Object o -> Object (KM.insert "usage" (toJSON usage) o)
    value -> value

usageArgument :: String -> Arguments Bool
usageArgument description = (/= Just False) <$> optionalArgument "usage" description

tenantUuidArgument :: Arguments U.UUID
tenantUuidArgument = requiredArgument "tenant_uuid" "The tenant"

tenantReadPermissions :: [String]
tenantReadPermissions = [_MCP_READ_ROLE_PERMISSION, _TENANTS_MANAGE_ROLE_PERMISSION]

tenantWritePermissions :: [String]
tenantWritePermissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _TENANTS_MANAGE_ROLE_PERMISSION]

tenantEntityName :: String
tenantEntityName = "tenant"
