module Specs.Service.Mcp.Tool.RoleTools.Common where

import qualified Data.UUID as U

import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Util.Uuid

-- --------------------------------
-- FIXTURES
-- --------------------------------
mcpPermissions :: [String]
mcpPermissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION]

newRoleUuid :: U.UUID
newRoleUuid = u' "3f2e1d0c-9b8a-4c7d-8e6f-5a4b3c2d1e0f"

newRole :: Role
newRole = dataStewardRole {uuid = newRoleUuid, name = "Agent admin", permissions = mcpPermissions, workspaceUuid = Nothing}
