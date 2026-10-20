module Shared.Service.Mcp.Tool.UserGroupTools where

import Control.Monad (unless, void)
import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Proxy
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.User.Group.UserGroupChangeDTO
import Shared.Api.Resource.User.Group.UserGroupChangeJM ()
import Shared.Api.Resource.User.Group.UserGroupDetailDTO
import Shared.Api.Resource.User.Group.UserGroupDetailJM ()
import Shared.Api.Resource.User.Group.UserGroupListJM ()
import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeDTO
import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeJM ()
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupList
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.User.Group.UserGroupMapper (toChangeDTO)
import Shared.Service.User.Group.UserGroupService
import Shared.Service.User.GroupMembership.UserGroupMembershipMapper (toCreateDTO)
import Shared.Service.User.GroupMembership.UserGroupMembershipService
import Shared.Util.JsonSchema

userGroupTools :: WizardRequestContextC s m => [McpTool m]
userGroupTools = [listUserGroupsTool, saveUserGroupTool, deleteUserGroupTool]

listUserGroupsTool :: forall s m. WizardRequestContextC s m => McpTool m
listUserGroupsTool =
  McpTool
    { name = "list_user_groups"
    , title = "List user groups"
    , description =
        "List the user groups you can see, optionally in one workspace, filtered by name and by whether you are a member (member); "
          ++ "editable tells whether you may change a group; save_user_group with only uuid returns a group with its members"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "userGroups" (jsonSchema (Proxy :: Proxy UserGroupList))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(inScope, mMember, (mQuery, pageable)) ->
        inScope (toJSON <$> getUserGroupsPage mQuery mMember pageable [Sort "name" Ascending])
    }
  where
    arguments =
      (,,)
        <$> scopeArguments
        <*> optionalArgument "member" "Only the groups you are (true) or are not (false) a member of"
        <*> searchArguments "Search in the names of the groups"
        :: Arguments (m Value -> m Value, Maybe Bool, (Maybe String, Pageable))

saveUserGroupTool :: forall s m. WizardRequestContextC s m => McpTool m
saveUserGroupTool =
  McpTool
    { name = "save_user_group"
    , title = "Save user group"
    , description =
        "Create a user group or change its name, description, privacy and members; only the fields you give change, and members, when given, replace all members "
          ++ "(userUuid and mType OwnerUserGroupMembershipType or MemberUserGroupMembershipType; every member must belong to the workspace of the group). "
          ++ "A new group needs name and private and is created in a workspace; without members you become its owner when you are a member of that workspace. "
          ++ uuidDescription "a group (with only uuid you get the group with its members)"
          ++ ". "
          ++ expectedUpdatedAtDescription "group"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy UserGroupDetailDTO)
    , annotations = writeAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, userGroupWritePermission]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(inScope, mUuid, mExpectedUpdatedAt, mMembers, changes) ->
        inScope (toJSON <$> saveUserGroup mUuid mExpectedUpdatedAt mMembers changes)
    }
  where
    arguments =
      (,,,,)
        <$> scopeArguments
        <*> uuidArgument userGroupEntityName
        <*> expectedUpdatedAtArgument
        <*> optionalArgument "members" "All members of the group"
        <*> changesArguments (jsonSchema (Proxy :: Proxy UserGroupChangeDTO))
        :: Arguments (m Value -> m Value, Maybe U.UUID, Maybe UTCTime, Maybe [UserGroupMembershipChangeDTO], Object)

saveUserGroup :: WizardRequestContextC s m => Maybe U.UUID -> Maybe UTCTime -> Maybe [UserGroupMembershipChangeDTO] -> Object -> m UserGroupDetailDTO
saveUserGroup mUuid mExpectedUpdatedAt mMembers changes =
  runInTransaction $ do
    userGroup <- saveByUuid findUserGroupByUuid' (createUserGroupFromChanges changes) (updateUserGroupFromChanges mExpectedUpdatedAt changes) mUuid
    traverse_ (setUserGroupMembers userGroup.uuid) mMembers
    getUserGroupByUuid userGroup.uuid

createUserGroupFromChanges :: WizardRequestContextC s m => Object -> U.UUID -> m UserGroup
createUserGroupFromChanges changes uuid = createFromChanges userGroupEntityName ["name", "private"] parseChanges (createUserGroupWithUuid uuid) changes

updateUserGroupFromChanges :: WizardRequestContextC s m => Maybe UTCTime -> Object -> UserGroup -> m UserGroup
updateUserGroupFromChanges mExpectedUpdatedAt changes userGroup = do
  let current = toChangeDTO userGroup
  changed <- mergeChangesSince userGroupEntityName mExpectedUpdatedAt userGroup.updatedAt changes current
  modifyIfChanged current changed (return userGroup) (modifyUserGroup userGroup.uuid)

setUserGroupMembers :: WizardRequestContextC s m => U.UUID -> [UserGroupMembershipChangeDTO] -> m ()
setUserGroupMembers userGroupUuid members = do
  memberships <- findUserGroupMembershipsByUserGroupUuid userGroupUuid
  let current = fmap toCreateDTO memberships
  unless (L.sortOn (.userUuid) current == L.sortOn (.userUuid) members) (void $ modifyUserGroupMembership userGroupUuid members)

deleteUserGroupTool :: WizardRequestContextC s m => McpTool m
deleteUserGroupTool =
  McpTool
    { name = "delete_user_group"
    , title = "Delete user group"
    , description = "Delete a user group with its memberships; the projects shared with the group are no longer shared with its members. It cannot be undone"
    , inputSchema = argumentsSchema userGroupUuidArgument
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, userGroupWritePermission]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments userGroupUuidArgument $ \userGroupUuid -> object [] <$ deleteUserGroup userGroupUuid
    }

userGroupUuidArgument :: Arguments U.UUID
userGroupUuidArgument = requiredArgument "user_group_uuid" "The user group"

userGroupEntityName :: String
userGroupEntityName = "user group"

userGroupWritePermission :: String
userGroupWritePermission = anyOfPermissions [_USER_GROUPS_CREATE_ROLE_PERMISSION, _USER_GROUPS_MANAGE_ROLE_PERMISSION]
