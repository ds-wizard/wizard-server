module Shared.Service.Mcp.Tool.WorkspaceTools where

import Data.Aeson
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.Mcp.Workspace.McpWorkspaceMembersDTO
import Shared.Api.Resource.Mcp.Workspace.McpWorkspaceMembersJM ()
import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceChangeJM ()
import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Api.Resource.Workspace.WorkspaceMemberChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceMemberJM ()
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Model.AuditLog.Data.WorkspaceAuditLogData
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMember
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Workspace.WorkspaceMapper (toChangeDTO, toMcpMemberDTO)
import Shared.Service.Workspace.WorkspaceService
import Shared.Util.JsonSchema
import Shared.Util.String (f')

workspaceTools :: WizardRequestContextC s m => [McpTool m]
workspaceTools = [listWorkspacesTool, listWorkspaceMembersTool, saveWorkspaceTool, setWorkspaceMembersTool, deleteWorkspaceTool]

listWorkspacesTool :: WizardRequestContextC s m => McpTool m
listWorkspacesTool =
  McpTool
    { name = "list_workspaces"
    , title = "List workspaces"
    , description = "List the workspaces with their updatedAt: all of them with workspaces.manage, otherwise those you are a member of"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "workspaces" (jsonSchema (Proxy :: Proxy Workspace))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(mQuery, pageable) -> toJSON <$> getWorkspacesPage mQuery pageable [Sort "name" Ascending]
    }
  where
    arguments = searchArguments "Search in the names of the workspaces"

listWorkspaceMembersTool :: forall s m. WizardRequestContextC s m => McpTool m
listWorkspaceMembersTool =
  McpTool
    { name = "list_workspace_members"
    , title = "List workspace members"
    , description = "List the members of a workspace with their workspace role, filtered by name and e-mail; set_workspace_members adds, changes and removes them"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "members" (jsonSchema (Proxy :: Proxy WorkspaceMember))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(workspaceUuid, (mQuery, pageable)) ->
        toJSON <$> getWorkspaceMembersPage workspaceUuid mQuery pageable [Sort "lastName" Ascending, Sort "firstName" Ascending]
    }
  where
    arguments =
      (,)
        <$> requiredArgument "workspace_uuid" "The workspace"
        <*> searchArguments "Search in the names and e-mails of the members"
        :: Arguments (U.UUID, (Maybe String, Pageable))

saveWorkspaceTool :: forall s m. WizardRequestContextC s m => McpTool m
saveWorkspaceTool =
  McpTool
    { name = "save_workspace"
    , title = "Save workspace"
    , description =
        "Create a workspace or change its name, description and primary color; only the fields you give change. "
          ++ "A new workspace needs name and can only be created in a tenant with more than one workspace (multiWorkspace in get_tenant_context); you become its admin. "
          ++ uuidDescription "a workspace"
          ++ ". "
          ++ expectedUpdatedAtDescription "workspace"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy Workspace)
    , annotations = writeAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _WORKSPACES_MANAGE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(mUuid, mExpectedUpdatedAt, changes) -> toJSON <$> saveWorkspace mUuid mExpectedUpdatedAt changes
    }
  where
    arguments =
      (,,)
        <$> uuidArgument workspaceEntityName
        <*> expectedUpdatedAtArgument
        <*> changesArguments (jsonSchema (Proxy :: Proxy WorkspaceChangeDTO))
        :: Arguments (Maybe U.UUID, Maybe UTCTime, Object)

saveWorkspace :: WizardRequestContextC s m => Maybe U.UUID -> Maybe UTCTime -> Object -> m Workspace
saveWorkspace mUuid mExpectedUpdatedAt changes =
  runInTransaction $
    saveByUuid findWorkspaceByUuid' (createWorkspaceFromChanges changes) (updateWorkspaceFromChanges mExpectedUpdatedAt changes) mUuid

createWorkspaceFromChanges :: WizardRequestContextC s m => Object -> U.UUID -> m Workspace
createWorkspaceFromChanges changes uuid = createFromChanges workspaceEntityName ["name"] parseChanges (createWorkspaceWithUuid uuid) changes

updateWorkspaceFromChanges :: WizardRequestContextC s m => Maybe UTCTime -> Object -> Workspace -> m Workspace
updateWorkspaceFromChanges mExpectedUpdatedAt changes workspace = do
  let current = toChangeDTO workspace
  changed <- mergeChangesSince workspaceEntityName mExpectedUpdatedAt workspace.updatedAt changes current
  modifyIfChanged current changed (return workspace) (modifyWorkspace workspace.uuid)

setWorkspaceMembersTool :: forall s m. WizardRequestContextC s m => McpTool m
setWorkspaceMembersTool =
  McpTool
    { name = "set_workspace_members"
    , title = "Set workspace members"
    , description =
        "Add users to a workspace or set their workspace role (members), and remove members (remove); members you do not mention stay as they are. "
          ++ "roleUuid is a role of that workspace (list_roles with workspace_uuid); without it the member gets the default role of the workspace. "
          ++ "A removed member also leaves the user groups of the workspace and loses the projects of the workspace shared with them. "
          ++ "Only in a tenant with more than one workspace"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpWorkspaceMembersDTO)
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MEMBERS_MANAGE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(workspaceUuid, mMembers, mRemoved) ->
        toJSON <$> setWorkspaceMembers workspaceUuid (fromMaybe [] mMembers) (fromMaybe [] mRemoved)
    }
  where
    arguments =
      (,,)
        <$> requiredArgument "workspace_uuid" "The workspace"
        <*> optionalArgument "members" "The users to add to the workspace or whose workspace role to set"
        <*> optionalArgument "remove" "The users to remove from the workspace"
        :: Arguments (U.UUID, Maybe [McpWorkspaceMemberDTO], Maybe [U.UUID])

setWorkspaceMembers :: WizardRequestContextC s m => U.UUID -> [McpWorkspaceMemberDTO] -> [U.UUID] -> m McpWorkspaceMembersDTO
setWorkspaceMembers workspaceUuid members removed =
  runInTransaction $ do
    traverse_ (\member -> addWorkspaceMember workspaceUuid member.userUuid (WorkspaceMemberChangeDTO member.roleUuid)) members
    traverse_ (removeWorkspaceMember workspaceUuid) removed
    memberships <- traverse (findWorkspaceMembership workspaceUuid . (.userUuid)) members
    return $ McpWorkspaceMembersDTO (fmap toMcpMemberDTO memberships) removed

deleteWorkspaceTool :: WizardRequestContextC s m => McpTool m
deleteWorkspaceTool =
  McpTool
    { name = "delete_workspace"
    , title = "Delete workspace"
    , description =
        "Delete a workspace with its projects, documents, knowledge model editors, document template drafts and user groups; the last workspace cannot be deleted. "
          ++ "It cannot be undone. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments workspaceUuidArgument)
    , outputSchema = Just $ confirmationOutputSchema (objectSchema [] [])
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _WORKSPACES_MANAGE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "delete_workspace" workspaceUuidArgument prepareWorkspaceDeletion
    }

prepareWorkspaceDeletion :: WizardRequestContextC s m => U.UUID -> m (McpConfirmation m)
prepareWorkspaceDeletion uuid = do
  (workspace, counts) <- getWorkspaceDeletion uuid
  return
    McpConfirmation
      { summary =
          f'
            "Deletes the workspace %s with %s projects, %s documents, %s knowledge model editors, %s document template drafts and %s user groups; it cannot be undone"
            [workspace.name, show counts.projects, show counts.documents, show counts.knowledgeModelEditors, show counts.documentTemplateEditors, show counts.userGroups]
      , revision = iso8601Show workspace.updatedAt ++ concatMap show [counts.projects, counts.documents, counts.knowledgeModelEditors, counts.documentTemplateEditors, counts.userGroups]
      , perform = object [] <$ deleteWorkspace uuid
      }

workspaceUuidArgument :: Arguments U.UUID
workspaceUuidArgument = requiredArgument "workspace_uuid" "The workspace"

workspaceEntityName :: String
workspaceEntityName = "workspace"
