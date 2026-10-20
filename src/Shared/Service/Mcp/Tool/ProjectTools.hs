module Shared.Service.Mcp.Tool.ProjectTools where

import Control.Monad (void)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.Mcp.Project.McpProjectDTO
import Shared.Api.Resource.Mcp.Project.McpProjectDetailDTO
import Shared.Api.Resource.Mcp.Project.McpProjectDetailJM ()
import Shared.Api.Resource.Mcp.Project.McpProjectJM ()
import Shared.Api.Resource.Mcp.Project.McpProjectQuestionnaireDTO
import Shared.Api.Resource.Mcp.Project.McpProjectQuestionnaireJM ()
import Shared.Api.Resource.Mcp.Project.McpRepliesChangeDTO
import Shared.Api.Resource.Mcp.Project.McpRepliesChangeResultDTO
import Shared.Api.Resource.Mcp.Project.McpRepliesChangeResultJM ()
import Shared.Api.Resource.Project.ProjectCreateDTO
import Shared.Api.Resource.Project.ProjectCreateFromTemplateJM ()
import Shared.Api.Resource.Project.ProjectCreateJM ()
import Shared.Api.Resource.Project.ProjectDTO
import Shared.Api.Resource.Project.ProjectJM ()
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Api.Resource.Project.ProjectSettingsChangeDTO
import Shared.Api.Resource.Project.ProjectSettingsChangeJM ()
import Shared.Api.Resource.Project.ProjectShareChangeDTO
import Shared.Api.Resource.Project.ProjectShareChangeJM ()
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.Project.Project
import Shared.Service.Mcp.Tool.KnowledgeModelMigrationTools (migrateProjectTool)
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Project.Event.ProjectEventService
import Shared.Service.Project.ProjectMapper (toChangeDTO, toMcpProjectDTO, toSettingsChangeDTO)
import Shared.Service.Project.ProjectService
import Shared.Util.JsonSchema
import Shared.Util.Reference (parseReference)

projectTools :: WizardRequestContextC s m => [McpTool m]
projectTools =
  [ listProjectsTool
  , getProjectTool
  , createProjectTool
  , cloneProjectTool
  , getProjectQuestionnaire
  , setProjectReplies
  , updateProjectSettings
  , setProjectSharingTool
  , migrateProjectTool
  , deleteProjectTool
  ]

listProjectsTool :: forall s m. WizardRequestContextC s m => McpTool m
listProjectsTool =
  McpTool
    { name = "list_projects"
    , title = "List projects"
    , description =
        "List the projects you can see, most recently changed first, optionally in one workspace, only project templates (is_template) "
          ++ "or only those with any of the project tags, users, user groups or knowledge model package ids; get_project shows one with its permissions"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "projects" (jsonSchema (Proxy :: Proxy ProjectDTO))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \((inScope, mIsTemplate, mTags), (mUsers, mGroups, mPackages), (mQuery, pageable)) ->
        inScope . fmap toJSON $
          getProjectsForCurrentUserPageDto mQuery mIsTemplate mTags Nothing mUsers Nothing mGroups Nothing mPackages Nothing pageable [Sort "updatedAt" Descending]
    }
  where
    arguments =
      (,,)
        <$> ((,,) <$> scopeArguments <*> optionalArgument "is_template" "Only project templates (true) or only other projects (false)" <*> optionalArgument "project_tags" "Project tags")
        <*> ((,,) <$> optionalArgument "user_uuids" "Users with access" <*> optionalArgument "user_group_uuids" "User groups with access" <*> packageIdsArgument)
        <*> searchArguments "Search in the names of the projects"
        :: Arguments ((m Value -> m Value, Maybe Bool, Maybe [String]), (Maybe [U.UUID], Maybe [U.UUID], Maybe [Coordinate]), (Maybe String, Pageable))

packageIdsArgument :: Arguments (Maybe [Coordinate])
packageIdsArgument = checkArguments (traverse (traverse toCoordinate)) (optionalArgument name "Knowledge model package ids such as org:km:1.0.0")
  where
    name = "knowledge_model_package_ids"
    toCoordinate reference = maybe (Left $ _ERROR_VALIDATION__MCP_INVALID_ARGUMENT name reference) Right (parseReference reference)

getProjectTool :: WizardRequestContextC s m => McpTool m
getProjectTool =
  McpTool
    { name = "get_project"
    , title = "Get project"
    , description =
        "Get a project with its settings and updatedAt, the permissions of its users and user groups, "
          ++ "and whether its knowledge model package and document template are up to date"
    , inputSchema = argumentsSchema projectUuidArgument
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpProjectDetailDTO)
    , annotations = readOnlyAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = NoTransaction
    , run = withArguments projectUuidArgument $ fmap toJSON . getMcpProjectDetailByUuid
    }

createProjectTool :: forall s m. WizardRequestContextC s m => McpTool m
createProjectTool =
  McpTool
    { name = "create_project"
    , title = "Create project"
    , description =
        "Create a project in a workspace from a knowledge model package, optionally with question tags (questionTagUuids), a document template and its format, "
          ++ "for example as test data for previews; or from a project template (project_template_uuid), which copies its replies, files and settings and needs only name. Give uuid with a new UUID to make the create safe to repeat: a project that already has that uuid is returned as it is"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpProjectDTO)
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _PROJECTS_CREATE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(inScope, mUuid, mTemplateUuid, changes) -> inScope (toJSON <$> createProjectFromChanges mUuid mTemplateUuid changes)
    }
  where
    arguments =
      (,,,)
        <$> scopeArguments
        <*> newUuidArgument
        <*> optionalArgument "project_template_uuid" "The project template to create the project from (list_projects with is_template)"
        <*> changesArguments (jsonSchema (Proxy :: Proxy ProjectCreateDTO))
        :: Arguments (m Value -> m Value, Maybe U.UUID, Maybe U.UUID, Object)

createProjectFromChanges :: WizardRequestContextC s m => Maybe U.UUID -> Maybe U.UUID -> Object -> m McpProjectDTO
createProjectFromChanges mUuid mTemplateUuid changes = runInTransaction $ saveByUuid findProjectByUuid' (createNewProject mTemplateUuid changes) existingProject mUuid

createNewProject :: WizardRequestContextC s m => Maybe U.UUID -> Object -> U.UUID -> m McpProjectDTO
createNewProject (Just templateUuid) changes uuid = do
  requireChanges projectEntityName ["name"] changes
  reqDto <- parseChanges (KM.insert "projectUuid" (toJSON templateUuid) changes)
  _ <- createProjectFromTemplateWithUuid reqDto uuid
  toMcpProjectDTO <$> findProjectByUuid uuid
createNewProject Nothing changes uuid = do
  requireChanges projectEntityName ["name", "knowledgeModelPackageUuid"] changes
  reqDto <- parseChanges (KM.union changes projectCreateDefaults)
  _ <- createProjectWithGivenUuid reqDto uuid
  toMcpProjectDTO <$> findProjectByUuid uuid

existingProject :: WizardRequestContextC s m => Project -> m McpProjectDTO
existingProject project = toMcpProjectDTO <$> getProjectByUuid project.uuid

newUuidArgument :: Arguments (Maybe U.UUID)
newUuidArgument = optionalArgument "uuid" "The UUID the new project gets, so that repeating the same call creates it only once"

cloneProjectTool :: WizardRequestContextC s m => McpTool m
cloneProjectTool =
  McpTool
    { name = "clone_project"
    , title = "Clone project"
    , description =
        "Copy a project with its replies, versions, comments and files to a new project named \"Copy of <name>\" in the same workspace, with you as an owner. "
          ++ "Give uuid with a new UUID to make the copy safe to repeat"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpProjectDTO)
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _PROJECTS_CREATE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(projectUuid, mUuid) -> toJSON <$> cloneProjectByUuid projectUuid mUuid
    }
  where
    arguments = (,) <$> projectUuidArgument <*> newUuidArgument

cloneProjectByUuid :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m McpProjectDTO
cloneProjectByUuid projectUuid = runInTransaction . saveByUuid findProjectByUuid' cloneNew existingProject
  where
    cloneNew uuid = cloneProjectWithUuid projectUuid uuid >> toMcpProjectDTO <$> findProjectByUuid uuid

projectCreateDefaults :: Object
projectCreateDefaults =
  KM.fromList
    [ ("visibility", toJSON PrivateProjectVisibility)
    , ("sharing", toJSON RestrictedProjectSharing)
    , ("questionTagUuids", toJSON ([] :: [U.UUID]))
    ]

getProjectQuestionnaire :: WizardRequestContextC s m => McpTool m
getProjectQuestionnaire =
  markThirdPartyFields [["replies"]] $
    McpTool
      { name = "get_project_questionnaire"
      , title = "Get project questionnaire"
      , description =
          "Get the current replies of a project by their paths (get_authoring_guide topic document-context), its phase and labels, and the project with its knowledge model package "
            ++ "and question tags; read the questions with get_knowledge_model_tree of that package"
      , inputSchema = argumentsSchema projectUuidArgument
      , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpProjectQuestionnaireDTO)
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just ProjectsMcpToolset
      , transactionState = NoTransaction
      , run = withArguments projectUuidArgument $ fmap toJSON . getMcpProjectQuestionnaireByUuid
      }

setProjectReplies :: WizardRequestContextC s m => McpTool m
setProjectReplies =
  McpTool
    { name = "set_project_replies"
    , title = "Set project replies"
    , description =
        "Set replies of a project by path (get_authoring_guide topic document-context) and clear the replies of the paths in clear; replies at other paths stay as they are. "
          ++ "List items are UUIDs you choose in the ItemListReply of the list question; use them in the paths of the questions of the items. "
          ++ "Every reply must fit the knowledge model of the project and the validations of its question; otherwise nothing is stored. People with the project open see the change live"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpRepliesChangeResultDTO)
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ fmap toJSON . uncurry modifyProjectReplies
    }
  where
    arguments = (,) <$> projectUuidArgument <*> repliesChangeArguments

updateProjectSettings :: forall s m. WizardRequestContextC s m => McpTool m
updateProjectSettings =
  McpTool
    { name = "update_project_settings"
    , title = "Update project settings"
    , description =
        "Change the settings of a project: its name, description, projectTags, document template and format, languages, and whether it is a project template "
          ++ "(isTemplate, which needs projects.manageTemplates); only the fields you give change. "
          ++ expectedUpdatedAtDescription "project"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpProjectDTO)
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(projectUuid, mExpectedUpdatedAt, changes) -> toJSON <$> updateSettings projectUuid mExpectedUpdatedAt changes
    }
  where
    arguments =
      (,,)
        <$> projectUuidArgument
        <*> expectedUpdatedAtArgument
        <*> changesArguments (jsonSchema (Proxy :: Proxy ProjectSettingsChangeDTO))
        :: Arguments (U.UUID, Maybe UTCTime, Object)

updateSettings :: WizardRequestContextC s m => U.UUID -> Maybe UTCTime -> Object -> m McpProjectDTO
updateSettings projectUuid mExpectedUpdatedAt changes =
  runInTransaction $ do
    project <- getProjectByUuid projectUuid
    checkUnchangedSince projectEntityName mExpectedUpdatedAt project.updatedAt
    modifyProjectSettingsIfChanged project =<< mergeChanges changes (toSettingsChangeDTO project)
    toMcpProjectDTO <$> getProjectByUuid projectUuid

setProjectSharingTool :: forall s m. WizardRequestContextC s m => McpTool m
setProjectSharingTool =
  McpTool
    { name = "set_project_sharing"
    , title = "Set project sharing"
    , description =
        "Change who can reach a project: visibility, sharing and the permissions of users and user groups, "
          ++ "where perms is cumulative, [\"VIEW\", \"COMMENT\", \"EDIT\", \"ADMIN\"] for an owner. Only the fields you give change; permissions replace all current ones. "
          ++ "Needs ADMIN on the project; new members get an invitation e-mail. "
          ++ expectedUpdatedAtDescription "project"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpProjectDetailDTO)
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(projectUuid, mExpectedUpdatedAt, changes) -> toJSON <$> setSharing projectUuid mExpectedUpdatedAt changes
    }
  where
    arguments =
      (,,)
        <$> projectUuidArgument
        <*> expectedUpdatedAtArgument
        <*> changesArguments (jsonSchema (Proxy :: Proxy ProjectShareChangeDTO))
        :: Arguments (U.UUID, Maybe UTCTime, Object)

setSharing :: WizardRequestContextC s m => U.UUID -> Maybe UTCTime -> Object -> m McpProjectDetailDTO
setSharing projectUuid mExpectedUpdatedAt changes =
  runInTransaction $ do
    project <- getProjectByUuid projectUuid
    let current = toChangeDTO project
    changed <- mergeChangesSince projectEntityName mExpectedUpdatedAt project.updatedAt changes current
    modifyIfChanged current changed (return ()) (void . modifyProjectShare projectUuid)
    getMcpProjectDetailByUuid projectUuid

deleteProjectTool :: WizardRequestContextC s m => McpTool m
deleteProjectTool =
  McpTool
    { name = "delete_project"
    , title = "Delete project"
    , description =
        "Delete a project with all its replies, versions, comments, documents and files; needs ADMIN on the project. It cannot be undone. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments projectUuidArgument)
    , outputSchema = Just $ confirmationOutputSchema (objectSchema [] [])
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "delete_project" projectUuidArgument prepareProjectDeletion
    }

prepareProjectDeletion :: WizardRequestContextC s m => U.UUID -> m (McpConfirmation m)
prepareProjectDeletion uuid = do
  checkDeletePermissionToProject uuid
  project <- getProjectByUuid uuid
  return
    McpConfirmation
      { summary = "Deletes the project " ++ project.name ++ " with all its replies, versions, comments, documents and files; it cannot be undone"
      , revision = iso8601Show project.updatedAt
      , perform = object [] <$ deleteProject uuid True
      }

projectUuidArgument :: Arguments U.UUID
projectUuidArgument = requiredArgument "project_uuid" "The project"

repliesChangeArguments :: Arguments McpRepliesChangeDTO
repliesChangeArguments =
  McpRepliesChangeDTO
    . fromMaybe M.empty
    <$> optionalArgument "replies" "The replies by path, for example {\"<chapter>.<question>\": {\"type\": \"StringReply\", \"value\": \"...\"}}"
    <*> (fromMaybe [] <$> optionalArgument "clear" "The paths whose replies to clear")

projectEntityName :: String
projectEntityName = "project"
