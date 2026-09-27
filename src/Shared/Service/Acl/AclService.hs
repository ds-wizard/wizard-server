module Shared.Service.Acl.AclService where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Model.Context.Scope
import Shared.Model.User.RoleSimple

class AclContext m where
  hasPermission :: String -> m Bool
  checkPermission :: String -> m ()
  checkPermissionsAny :: [String] -> m ()
  checkPermissionsAll :: [String] -> m ()
  hasPermissionInWorkspace :: String -> Maybe U.UUID -> m Bool
  checkPermissionInWorkspace :: String -> Maybe U.UUID -> m ()

effectivePermissions :: UserDTO -> M.Map U.UUID RoleSimple -> Bool -> Scope -> [String]
effectivePermissions user workspaceRoles multiWorkspace scope =
  case scope of
    WorkspaceScope workspaceUuid
      | M.member workspaceUuid workspaceRoles -> permissionsInWorkspace user workspaceRoles multiWorkspace (Just workspaceUuid)
    TenantScope -> permissionsInWorkspace user workspaceRoles multiWorkspace Nothing
    _ -> user.role.permissions ++ concatMap (.permissions) (M.elems workspaceRoles)

permissionsInWorkspace :: UserDTO -> M.Map U.UUID RoleSimple -> Bool -> Maybe U.UUID -> [String]
permissionsInWorkspace user workspaceRoles multiWorkspace mWorkspaceUuid =
  user.role.permissions ++ case mWorkspaceUuid of
    Just workspaceUuid -> workspacePermissions workspaceRoles workspaceUuid
    Nothing
      | multiWorkspace -> []
      | otherwise -> concatMap (.permissions) (M.elems workspaceRoles)

workspacePermissions :: M.Map U.UUID RoleSimple -> U.UUID -> [String]
workspacePermissions workspaceRoles workspaceUuid = maybe [] (.permissions) (M.lookup workspaceUuid workspaceRoles)
