module Shared.Service.Mcp.Tool.UserTools where

import Control.Monad (unless, when)
import Data.Aeson
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as U
import GHC.Records (HasField)

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.Mcp.User.McpUserInviteDTO
import Shared.Api.Resource.Mcp.User.McpUserInviteJM ()
import Shared.Api.Resource.User.RoleListJM ()
import Shared.Database.DAO.Settings.SettingsRolesDAO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.Settings.Settings
import Shared.Model.User.Role
import Shared.Model.User.RoleList
import Shared.Service.Mcp.Tool.McpAdminLogin (checkAdminLoginRemainsForUser)
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpExecutableServices
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Settings.SettingsService
import Shared.Util.JsonSchema

searchUsersTool :: forall s m user. (WizardRequestContextC s m, ToJSON user, JsonSchema user) => (Maybe String -> Maybe String -> Pageable -> [Sort] -> m (Page user)) -> McpTool m
searchUsersTool getUsersPage =
  McpTool
    { name = "search_users"
    , title = "Search users"
    , description = "Search the users of the organization by name or e-mail, optionally only those with one organization role (list_roles with tenant: true shows the roles)"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "users" (jsonSchema (Proxy :: Proxy user))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION, _USERS_MANAGE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(mRoleUuid, (mQuery, pageable)) ->
        toJSON <$> getUsersPage mQuery (fmap U.toString mRoleUuid) pageable [Sort "lastName" Ascending, Sort "firstName" Ascending]
    }
  where
    arguments =
      (,)
        <$> optionalArgument "role_uuid" "Only the users with this organization role"
        <*> searchArguments "Search in the names and e-mails of the users"
        :: Arguments (Maybe U.UUID, (Maybe String, Pageable))

updateUserTool
  :: forall s m user change
   . ( WizardRequestContextC s m
     , ToJSON user
     , JsonSchema user
     , HasField "updatedAt" user UTCTime
     , ToJSON change
     , FromJSON change
     , JsonSchema change
     , HasField "roleUuid" change U.UUID
     , HasField "email" change String
     , HasField "active" change Bool
     )
  => (U.UUID -> m user)
  -> (user -> change)
  -> (U.UUID -> change -> m user)
  -> (U.UUID -> m [U.UUID])
  -> McpTool m
updateUserTool getUser toChange modifyUser findUserLoginProviders =
  McpTool
    { name = "update_user"
    , title = "Update user"
    , description =
        "Change the name or affiliation of a user, activate or deactivate them (active), or change their e-mail or their organization role (roleUuid), both of which need mcp.sensitive; "
          ++ "only the fields you give change. "
          ++ expectedUpdatedAtDescription "user"
          ++ "; "
          ++ "a call with only user_uuid returns the user. Deactivating an admin or taking the admin role away is refused when no other active admin could log in"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy user)
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _USERS_MANAGE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(userUuid, mExpectedUpdatedAt, changes) ->
        toJSON <$> updateUser getUser toChange modifyUser findUserLoginProviders userUuid mExpectedUpdatedAt changes
    }
  where
    arguments =
      (,,)
        <$> userUuidArgument
        <*> expectedUpdatedAtArgument
        <*> changesArguments (jsonSchema (Proxy :: Proxy change))
        :: Arguments (U.UUID, Maybe UTCTime, Object)

updateUser
  :: ( WizardRequestContextC s m
     , HasField "updatedAt" user UTCTime
     , ToJSON change
     , FromJSON change
     , HasField "roleUuid" change U.UUID
     , HasField "email" change String
     , HasField "active" change Bool
     )
  => (U.UUID -> m user)
  -> (user -> change)
  -> (U.UUID -> change -> m user)
  -> (U.UUID -> m [U.UUID])
  -> U.UUID
  -> Maybe UTCTime
  -> Object
  -> m user
updateUser getUser toChange modifyUser findUserLoginProviders userUuid mExpectedUpdatedAt changes =
  runInTransaction $ do
    user <- getUser userUuid
    let current = toChange user
    changed <- mergeChangesSince "user" mExpectedUpdatedAt user.updatedAt changes current
    when (changed.roleUuid /= current.roleUuid || changed.email /= current.email) (checkPermission _MCP_SENSITIVE_ROLE_PERMISSION)
    checkAdminLoginRemainsForUser findUserLoginProviders userUuid (current.active, current.roleUuid) (changed.active, changed.roleUuid)
    modifyIfChanged current changed (return user) (modifyUser userUuid)

inviteUsersTool :: forall s m user. (WizardRequestContextC s m, ToJSON user, JsonSchema user) => ([McpUserInviteDTO] -> m [user]) -> McpTool m
inviteUsersTool inviteUsers =
  McpTool
    { name = "invite_users"
    , title = "Invite users"
    , description =
        "Create accounts for new users and send each of them an e-mail with a link to set their password; "
          ++ "an organization role other than the default role of new users (roleUuid) needs mcp.sensitive. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments arguments)
    , outputSchema = Just $ confirmationOutputSchema (objectSchema ["users"] [("users", arraySchema (jsonSchema (Proxy :: Proxy user)))])
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _USERS_MANAGE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "invite_users" arguments (prepareInvitations inviteUsers)
    }
  where
    arguments = requiredArgument "users" "The users to invite" :: Arguments [McpUserInviteDTO]

prepareInvitations :: (WizardRequestContextC s m, ToJSON user) => ([McpUserInviteDTO] -> m [user]) -> [McpUserInviteDTO] -> m (McpConfirmation m)
prepareInvitations inviteUsers invites = do
  settingsRoles <- getCurrentSettings findSettingsRoles
  unless (all (maybe True (== settingsRoles.defaultRoleUuid) . (.roleUuid)) invites) (checkPermission _MCP_SENSITIVE_ROLE_PERMISSION)
  roles <- traverse (findRoleByUuid . fromMaybe settingsRoles.defaultRoleUuid . (.roleUuid)) invites
  return
    McpConfirmation
      { summary = "Invites " ++ show (length invites) ++ " users, each gets an e-mail with a link to set their password: " ++ intercalate ", " (zipWith inviteSummary invites roles)
      , revision = ""
      , perform = (\users -> object ["users" .= users]) <$> inviteUsers invites
      }

inviteSummary :: McpUserInviteDTO -> Role -> String
inviteSummary invite role = invite.email ++ " (" ++ role.name ++ ")"

deleteUserTool
  :: ( WizardRequestContextC s m
     , HasField "firstName" user String
     , HasField "lastName" user String
     , HasField "email" user String
     , HasField "active" user Bool
     , HasField "role" user RoleSimple
     , HasField "updatedAt" user UTCTime
     )
  => (U.UUID -> m user)
  -> McpExecutableServices m
  -> McpTool m
deleteUserTool getUser services =
  McpTool
    { name = "delete_user"
    , title = "Delete user"
    , description =
        "Delete a user account with its tokens, group memberships and project permissions; it cannot be undone. "
          ++ "Deleting an admin is refused when no other active admin could log in. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments userUuidArgument)
    , outputSchema = Just $ confirmationOutputSchema (objectSchema [] [])
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION, _USERS_MANAGE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "delete_user" userUuidArgument (prepareUserDeletion getUser services)
    }

prepareUserDeletion
  :: ( WizardRequestContextC s m
     , HasField "firstName" user String
     , HasField "lastName" user String
     , HasField "email" user String
     , HasField "active" user Bool
     , HasField "role" user RoleSimple
     , HasField "updatedAt" user UTCTime
     )
  => (U.UUID -> m user)
  -> McpExecutableServices m
  -> U.UUID
  -> m (McpConfirmation m)
prepareUserDeletion getUser services userUuid = do
  user <- getUser userUuid
  checkAdminLoginRemainsForUser services.findUserLoginProviders userUuid (user.active, user.role.uuid) (False, user.role.uuid)
  return
    McpConfirmation
      { summary = "Deletes the user " ++ user.firstName ++ " " ++ user.lastName ++ " (" ++ user.email ++ "); it cannot be undone"
      , revision = iso8601Show user.updatedAt
      , perform = object [] <$ services.deleteUser userUuid
      }

userUuidArgument :: Arguments U.UUID
userUuidArgument = requiredArgument "user_uuid" "The user"

listRolesTool :: forall s m. WizardRequestContextC s m => (Maybe String -> Pageable -> [Sort] -> m (Page RoleList)) -> McpTool m
listRolesTool getRolesPage =
  McpTool
    { name = "list_roles"
    , title = "List roles"
    , description =
        "List the roles of the organization (tenant: true) or of a workspace (workspace_uuid) with their permissions and the number of users who have them; "
          ++ "organization roles are assigned to users, workspace roles to the members of a workspace"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "roles" (jsonSchema (Proxy :: Proxy RoleList))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(inScope, (mQuery, pageable)) -> inScope (toJSON <$> getRolesPage mQuery pageable [Sort "name" Ascending])
    }
  where
    arguments = (,) <$> scopeArguments <*> searchArguments "Search in the names of the roles" :: Arguments (m Value -> m Value, (Maybe String, Pageable))
