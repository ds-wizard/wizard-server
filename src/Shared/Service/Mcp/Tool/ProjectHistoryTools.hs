module Shared.Service.Mcp.Tool.ProjectHistoryTools where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.Project.Event.ProjectEventJM ()
import Shared.Api.Resource.Project.Event.ProjectEventListJM ()
import Shared.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Shared.Api.Resource.Project.Version.ProjectVersionListJM ()
import Shared.Api.Resource.Project.Version.ProjectVersionRevertDTO
import Shared.Localization.Messages.Public
import Shared.Model.Common.Lens
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.Project.Event.ProjectEventList
import Shared.Model.Project.Event.ProjectEventListLenses ()
import Shared.Model.Project.Project
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Mcp.Tool.ProjectTools (projectUuidArgument)
import Shared.Service.Project.ProjectService
import Shared.Service.Project.Version.ProjectVersionService
import Shared.Util.JsonSchema

projectHistoryTools :: WizardRequestContextC s m => [McpTool m]
projectHistoryTools =
  [ listProjectVersionsTool
  , saveProjectVersionTool
  , deleteProjectVersionTool
  , listProjectEventsTool
  , getProjectEventTool
  , revertProjectTool
  ]

listProjectVersionsTool :: WizardRequestContextC s m => McpTool m
listProjectVersionsTool =
  markThirdPartyFields [["versions", "description"]] $
    McpTool
      { name = "list_project_versions"
      , title = "List project versions"
      , description = "List the named versions of a project, each pointing at the event (list_project_events) it marks"
      , inputSchema = argumentsSchema projectUuidArgument
      , outputSchema = Just $ objectSchema ["versions"] [("versions", arraySchema (jsonSchema (Proxy :: Proxy ProjectVersionList)))]
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just ProjectsMcpToolset
      , transactionState = NoTransaction
      , run = withArguments projectUuidArgument $ fmap (\versions -> object ["versions" .= versions]) . getVersions
      }

saveProjectVersionTool :: forall s m. WizardRequestContextC s m => McpTool m
saveProjectVersionTool =
  McpTool
    { name = "save_project_version"
    , title = "Save project version"
    , description =
        "Name a state of a project as a version (at event_uuid, the latest event by default), or rename a version by its uuid; needs ADMIN on the project. "
          ++ uuidDescription "a version"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy ProjectVersionList)
    , annotations = writeAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ fmap toJSON . saveVersion
    }
  where
    arguments =
      SaveVersionArguments
        <$> projectUuidArgument
        <*> uuidArgument "version"
        <*> requiredArgument "name" "The name of the version"
        <*> optionalArgument "description" "The description of the version"
        <*> optionalArgument "event_uuid" "The event the version marks"

data SaveVersionArguments = SaveVersionArguments
  { projectUuid :: U.UUID
  , uuid :: Maybe U.UUID
  , name :: String
  , description :: Maybe String
  , eventUuid :: Maybe U.UUID
  }

saveVersion :: WizardRequestContextC s m => SaveVersionArguments -> m ProjectVersionList
saveVersion args = saveByUuid findVersion createNew rename args.uuid
  where
    findVersion uuid = L.find ((== uuid) . (.uuid)) <$> getVersions args.projectUuid
    createNew uuid = do
      eventUuid <- maybe (latestEventUuid args.projectUuid) return args.eventUuid
      createVersionWithUuid args.projectUuid (ProjectVersionChangeDTO args.name args.description eventUuid) uuid
    rename version =
      modifyVersion
        args.projectUuid
        version.uuid
        (ProjectVersionChangeDTO args.name (args.description <|> version.description) (fromMaybe version.eventUuid args.eventUuid))

latestEventUuid :: WizardRequestContextC s m => U.UUID -> m U.UUID
latestEventUuid projectUuid = do
  page <- getProjectEventsPage projectUuid (Pageable (Just 0) (Just 1)) [Sort "createdAt" Descending]
  case page.entities of
    event : _ -> return (getUuid event)
    [] -> throwError . UserError $ _ERROR_VALIDATION__ABSENCE "project event"

deleteProjectVersionTool :: WizardRequestContextC s m => McpTool m
deleteProjectVersionTool =
  McpTool
    { name = "delete_project_version"
    , title = "Delete project version"
    , description = "Delete a named version of a project; its event and the replies stay. Needs ADMIN on the project"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(projectUuid, versionUuid) -> object [] <$ deleteVersion projectUuid versionUuid
    }
  where
    arguments = (,) <$> projectUuidArgument <*> requiredArgument "version_uuid" "The version"

listProjectEventsTool :: WizardRequestContextC s m => McpTool m
listProjectEventsTool =
  markThirdPartyFields [["_embedded", "projectEvents", "value"]] $
    McpTool
      { name = "list_project_events"
      , title = "List project events"
      , description = "List the history of a project, newest first: every reply set or cleared, phase and label change, with who made it and when"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just $ pageSchema "projectEvents" (jsonSchema (Proxy :: Proxy ProjectEventList))
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just ProjectsMcpToolset
      , transactionState = NoTransaction
      , run = withArguments arguments $ \(projectUuid, pageable) -> toJSON <$> getProjectEventsPage projectUuid pageable [Sort "createdAt" Descending]
      }
  where
    arguments = (,) <$> projectUuidArgument <*> pageableArguments

getProjectEventTool :: WizardRequestContextC s m => McpTool m
getProjectEventTool =
  markThirdPartyFields [["value"]] $
    McpTool
      { name = "get_project_event"
      , title = "Get project event"
      , description = "Get one event of the history of a project"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just $ describedSchema "The event, shaped as an item of list_project_events" (typeSchema "object")
      , annotations = readOnlyAnnotations
      , continuable = False
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just ProjectsMcpToolset
      , transactionState = NoTransaction
      , run = withArguments arguments $ fmap toJSON . uncurry getProjectEventForProjectUuid
      }
  where
    arguments = (,) <$> projectUuidArgument <*> eventUuidArgument

revertProjectTool :: WizardRequestContextC s m => McpTool m
revertProjectTool =
  McpTool
    { name = "revert_project"
    , title = "Revert project"
    , description =
        "Return a project to the state right after one of its events (list_project_events): the later events, the versions that mark them "
          ++ "and the files uploaded after it are deleted, and people with the project open are disconnected. Needs ADMIN on the project. It cannot be undone. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments arguments)
    , outputSchema = Just $ confirmationOutputSchema (objectSchema [] [])
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "revert_project" arguments (uncurry prepareRevert)
    }
  where
    arguments = (,) <$> projectUuidArgument <*> eventUuidArgument

prepareRevert :: WizardRequestContextC s m => U.UUID -> U.UUID -> m (McpConfirmation m)
prepareRevert projectUuid eventUuid = do
  (project, events, versions, files) <- getRevertedChanges projectUuid eventUuid
  return
    McpConfirmation
      { summary =
          "Reverts the project "
            ++ project.name
            ++ ": deletes "
            ++ show (length events)
            ++ " later events, "
            ++ show (length versions)
            ++ " versions ("
            ++ L.intercalate ", " (fmap (.name) versions)
            ++ ") and "
            ++ show (length files)
            ++ " files; it cannot be undone"
      , revision = iso8601Show project.updatedAt ++ show (length events)
      , perform = object [] <$ revertToEvent projectUuid (ProjectVersionRevertDTO eventUuid) True
      }

eventUuidArgument :: Arguments U.UUID
eventUuidArgument = requiredArgument "event_uuid" "The event"
