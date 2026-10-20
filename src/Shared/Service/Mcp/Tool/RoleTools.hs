module Shared.Service.Mcp.Tool.RoleTools where

import Data.Aeson
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.User.RoleChangeDTO
import Shared.Api.Resource.User.RoleChangeJM ()
import Shared.Api.Resource.User.RoleListJM ()
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.Role
import Shared.Model.User.RoleList
import Shared.Model.User.RolePermission
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.User.RoleMapper (toChangeDTO)
import Shared.Util.JsonSchema

roleTools :: WizardRequestContextC s m => (U.UUID -> RoleChangeDTO -> m RoleList) -> (U.UUID -> RoleChangeDTO -> m RoleList) -> (U.UUID -> m ()) -> [McpTool m]
roleTools createRole modifyRole deleteRole = [saveRoleTool createRole modifyRole, deleteRoleTool deleteRole]

saveRoleTool :: forall s m. WizardRequestContextC s m => (U.UUID -> RoleChangeDTO -> m RoleList) -> (U.UUID -> RoleChangeDTO -> m RoleList) -> McpTool m
saveRoleTool createRole modifyRole =
  McpTool
    { name = "save_role"
    , title = "Save role"
    , description =
        "Create a role of the organization (tenant: true) or of a workspace (workspace_uuid), or change the name or permissions of a role; only the fields you give change, "
          ++ "and permissions, when given, replace all permissions of the role and of everyone who has it (list_roles shows the roles with their permissions). "
          ++ "A new role needs name and permissions; workspace roles cannot carry organization permissions such as mcp.read, and the admin role cannot be changed. "
          ++ uuidDescription "a role"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy RoleList)
    , annotations = writeAnnotations
    , continuable = False
    , permissions = rolePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(inScope, mUuid, changes) -> inScope (toJSON <$> saveRole createRole modifyRole mUuid changes)
    }
  where
    arguments =
      (,,)
        <$> scopeArguments
        <*> uuidArgument roleEntityName
        <*> changesArguments (jsonSchema (Proxy :: Proxy RoleChangeDTO))
        :: Arguments (m Value -> m Value, Maybe U.UUID, Object)

saveRole :: WizardRequestContextC s m => (U.UUID -> RoleChangeDTO -> m RoleList) -> (U.UUID -> RoleChangeDTO -> m RoleList) -> Maybe U.UUID -> Object -> m RoleList
saveRole createRole modifyRole mUuid changes =
  runInTransaction $ saveByUuid findRoleByUuid' (createRoleFromChanges createRole changes) (updateRoleFromChanges modifyRole changes) mUuid

createRoleFromChanges :: WizardRequestContextC s m => (U.UUID -> RoleChangeDTO -> m RoleList) -> Object -> U.UUID -> m RoleList
createRoleFromChanges createRole changes uuid = createFromChanges roleEntityName ["name", "permissions"] parseChanges (createRole uuid) changes

updateRoleFromChanges :: WizardRequestContextC s m => (U.UUID -> RoleChangeDTO -> m RoleList) -> Object -> Role -> m RoleList
updateRoleFromChanges modifyRole changes role = do
  let current = toChangeDTO role
  changed <- mergeChanges changes current
  modifyIfChanged current changed (findRoleListByUuid role.uuid) (modifyRole role.uuid)

deleteRoleTool :: WizardRequestContextC s m => (U.UUID -> m ()) -> McpTool m
deleteRoleTool deleteRole =
  McpTool
    { name = "delete_role"
    , title = "Delete role"
    , description = "Delete a role nobody has; the admin role and the default role of new users or members cannot be deleted. It cannot be undone"
    , inputSchema = argumentsSchema roleUuidArgument
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = rolePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments roleUuidArgument $ \uuid -> object [] <$ deleteRole uuid
    }

rolePermissions :: [String]
rolePermissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION, _ROLES_MANAGE_ROLE_PERMISSION]

roleUuidArgument :: Arguments U.UUID
roleUuidArgument = requiredArgument "role_uuid" "The role"

roleEntityName :: String
roleEntityName = "role"
