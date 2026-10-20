module Shared.Service.Mcp.Tool.KnowledgeModelMigrationTools where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationViewDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationViewJM ()
import Shared.Api.Resource.Mcp.KnowledgeModel.McpMigrationResolutionArgumentDTO
import Shared.Api.Resource.Mcp.KnowledgeModel.McpMigrationResolutionArgumentJM ()
import Shared.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Shared.Api.Resource.Project.Migration.ProjectMigrationResultDTO
import Shared.Api.Resource.Project.Migration.ProjectMigrationResultJM ()
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.Localization.LocaleRecord
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService
import Shared.Service.Mcp.Tool.KnowledgeModelEditorTools (editorUuidArgument)
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Project.Migration.ProjectMigrationMapper
import Shared.Service.Project.Migration.ProjectMigrationService
import Shared.Util.JsonSchema

knowledgeModelMigrationTools :: WizardRequestContextC s m => [McpTool m]
knowledgeModelMigrationTools =
  [ startKnowledgeModelMigration
  , getKnowledgeModelMigration
  , resolveKnowledgeModelMigration
  , cancelKnowledgeModelMigration
  ]

startKnowledgeModelMigration :: forall s m. WizardRequestContextC s m => McpTool m
startKnowledgeModelMigration =
  migrationTool
    McpTool
      { name = "start_knowledge_model_migration"
      , title = "Start knowledge model migration"
      , description =
          "Start upgrading the fork in a knowledge model editor to a newer version of the package it forks. "
            ++ "The upstream events are applied one by one; the migration stops at the first one that conflicts with the changes of the fork. "
            ++ "Continue with get_ and resolve_knowledge_model_migration, then publish the result with publish_knowledge_model_editor"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just compactMigrationViewSchema
      , annotations = writeAnnotations
      , continuable = False
      , permissions = editorWritePermissions
      , toolset = Just KmMcpToolset
      , transactionState = Transactional
      , run = withArguments arguments $ \(editorUuid, targetPackageUuid) -> do
          _ <- createMigration editorUuid (KnowledgeModelMigrationCreateDTO targetPackageUuid)
          toJSON <$> getCurrentMigrationView editorUuid
      }
  where
    arguments =
      (,) <$> editorUuidArgument <*> requiredArgument "target_package_uuid" "The newer package version of the forked package to upgrade to" :: Arguments (U.UUID, U.UUID)

getKnowledgeModelMigration :: WizardRequestContextC s m => McpTool m
getKnowledgeModelMigration =
  migrationTool
    McpTool
      { name = "get_knowledge_model_migration"
      , title = "Get knowledge model migration"
      , description =
          "Get the state of the migration in a knowledge model editor: in a conflict, the upstream event (its eventUuid resolves it), "
            ++ "the affected entity with its children before and after the event, and the progress, the conflict being number processedEvents + 1 of totalEvents. "
            ++ "Explain the conflict to the user before you resolve it"
      , inputSchema = argumentsSchema editorUuidArgument
      , outputSchema = Just migrationViewSchema
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION, _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION]
      , toolset = Just KmMcpToolset
      , transactionState = NoTransaction
      , run = withArguments editorUuidArgument (fmap toJSON . getCurrentMigrationView)
      }

resolveKnowledgeModelMigration :: forall s m. WizardRequestContextC s m => McpTool m
resolveKnowledgeModelMigration =
  migrationTool
    McpTool
      { name = "resolve_knowledge_model_migration"
      , title = "Resolve knowledge model migration"
      , description =
          "Resolve the current conflict of a knowledge model migration: apply takes over the upstream event, reject keeps the fork as it is "
            ++ "(both need event_uuid, the eventUuid of the current conflict); apply_all applies every remaining upstream event. "
            ++ "To adapt an upstream change, reject it and make the change in the editor after the migration is published. "
            ++ "Returns the state after the resolution, with the next conflict if there is one"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just compactMigrationViewSchema
      , annotations = writeAnnotations
      , continuable = False
      , permissions = editorWritePermissions
      , toolset = Just KmMcpToolset
      , transactionState = Transactional
      , run = withArguments arguments $ \(editorUuid, mResolution) -> do
          maybe (solveAllConflicts editorUuid) (solveConflictAndMigrate editorUuid) mResolution
          toJSON <$> getCurrentMigrationView editorUuid
      }
  where
    arguments = (,) <$> editorUuidArgument <*> resolutionArguments :: Arguments (U.UUID, Maybe KnowledgeModelMigrationResolutionDTO)

resolutionArguments :: Arguments (Maybe KnowledgeModelMigrationResolutionDTO)
resolutionArguments =
  checkArguments toResolution $
    (,)
      <$> requiredArgument "action" "apply or reject the current conflict, or apply_all"
      <*> optionalArgument "event_uuid" "The eventUuid of the current conflict, for apply and reject"

toResolution :: (McpMigrationResolutionArgumentDTO, Maybe U.UUID) -> Either LocaleRecord (Maybe KnowledgeModelMigrationResolutionDTO)
toResolution (ApplyAllMcpMigrationResolution, _) = Right Nothing
toResolution (ApplyMcpMigrationResolution, Just eventUuid) = Right . Just $ KnowledgeModelMigrationResolutionDTO eventUuid ApplyKnowledgeModelMigrationAction
toResolution (RejectMcpMigrationResolution, Just eventUuid) = Right . Just $ KnowledgeModelMigrationResolutionDTO eventUuid RejectKnowledgeModelMigrationAction
toResolution _ = Left _ERROR_VALIDATION__MCP_RESOLUTION_EVENT_UUID

cancelKnowledgeModelMigration :: WizardRequestContextC s m => McpTool m
cancelKnowledgeModelMigration =
  McpTool
    { name = "cancel_knowledge_model_migration"
    , title = "Cancel knowledge model migration"
    , description = "Cancel the migration in a knowledge model editor and drop its resolutions; the editor stays as it was before the migration"
    , inputSchema = argumentsSchema editorUuidArgument
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = editorWritePermissions
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withArguments editorUuidArgument $ \editorUuid -> object [] <$ deleteCurrentMigration editorUuid
    }

migrateProjectTool :: forall s m. WizardRequestContextC s m => McpTool m
migrateProjectTool =
  McpTool
    { name = "migrate_project"
    , title = "Migrate project"
    , description =
        "Move a project to another version of its knowledge model package, for example the one just published from a migration. "
          ++ "Replies of questions that no longer exist are dropped. The project keeps its question tags unless you give tag_uuids"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy ProjectMigrationResultDTO)
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(projectUuid, packageUuid, mTagUuids) -> toJSON <$> migrateProjectToPackage projectUuid packageUuid mTagUuids
    }
  where
    arguments =
      (,,)
        <$> requiredArgument "project_uuid" "The project to migrate"
        <*> requiredArgument "target_package_uuid" "The knowledge model package version to move the project to"
        <*> optionalArgument "tag_uuids" "The question tags of the project after the migration; empty means all questions"
        :: Arguments (U.UUID, U.UUID, Maybe [U.UUID])

migrateProjectToPackage :: WizardRequestContextC s m => U.UUID -> U.UUID -> Maybe [U.UUID] -> m ProjectMigrationResultDTO
migrateProjectToPackage projectUuid packageUuid mTagUuids = do
  project <- findProjectByUuid projectUuid
  toMigrationResultDTO <$> migrateProject projectUuid (ProjectMigrationCreateDTO packageUuid (fromMaybe project.selectedQuestionTagUuids mTagUuids))

migrationTool :: Functor m => McpTool m -> McpTool m
migrationTool = markThirdPartyFields [["conflict", "event"], ["conflict", "before"], ["conflict", "after"]]

migrationViewSchema :: Value
migrationViewSchema = jsonSchema (Proxy :: Proxy KnowledgeModelMigrationViewDTO)

compactMigrationViewSchema :: Value
compactMigrationViewSchema =
  withDefinitions
    [("knowledgeModelEntityNode", describedSchema "An entity with its children, as in the output schema of get_knowledge_model_migration" (object ["type" .= ("object" :: String)]))]
    migrationViewSchema

editorWritePermissions :: [String]
editorWritePermissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION]
