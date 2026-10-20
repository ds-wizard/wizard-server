module Shared.Service.Mcp.Tool.ProjectFileTools where

import Control.Monad (void)
import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.File.FileCreateDTO
import Shared.Api.Resource.Mcp.Project.McpRepliesChangeDTO
import Shared.Api.Resource.Mcp.TemporaryFile.McpTemporaryFileDTO
import Shared.Api.Resource.Mcp.TemporaryFile.McpTemporaryFileJM ()
import Shared.Api.Resource.Project.File.ProjectFileListJM ()
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.Project.File.ProjectFileList
import Shared.Model.Project.ProjectReply
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Mcp.Tool.ProjectTools (projectUuidArgument)
import Shared.Service.Project.Event.ProjectEventService
import Shared.Service.Project.File.ProjectFileService
import Shared.Util.JsonSchema

projectFileTools :: WizardRequestContextC s m => [McpTool m]
projectFileTools =
  [ listProjectFilesTool
  , uploadProjectFileTool
  , downloadProjectFileTool
  , deleteProjectFileTool
  ]

listProjectFilesTool :: WizardRequestContextC s m => McpTool m
listProjectFilesTool =
  McpTool
    { name = "list_project_files"
    , title = "List project files"
    , description = "List the files uploaded to the file questions of a project, newest first"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "projectFiles" (jsonSchema (Proxy :: Proxy ProjectFileList))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(projectUuid, (mQuery, pageable)) ->
        toJSON <$> getProjectFilesPage mQuery (Just projectUuid) pageable [Sort "createdAt" Descending]
    }
  where
    arguments = (,) <$> projectUuidArgument <*> searchArguments "Search in the file names"

uploadProjectFileTool :: WizardRequestContextC s m => McpTool m
uploadProjectFileTool =
  McpTool
    { name = "upload_project_file"
    , title = "Upload project file"
    , description =
        "Upload a file (base64, at most 10 MB) as the reply of a file question of a project at its reply path (get_authoring_guide topic document-context); "
          ++ "the question's maximum file size applies. Needs EDIT on the project"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy ProjectFileList)
    , annotations = writeAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(projectUuid, path, file) -> toJSON <$> uploadFile projectUuid path file
    }
  where
    arguments =
      (,,)
        <$> projectUuidArgument
        <*> requiredArgument "path" "The reply path of the file question"
        <*> ( FileCreateDTO
                <$> requiredArgument "file_name" "The file name"
                <*> requiredArgument "content_type" "The content type, for example application/pdf"
                <*> requiredBase64ContentArgument "content" "The file content in base64"
            )

uploadFile :: WizardRequestContextC s m => U.UUID -> String -> FileCreateDTO -> m ProjectFileList
uploadFile projectUuid path reqDto = do
  questionUuid <- checkFileReplyPath projectUuid path
  file <- createProjectFile projectUuid questionUuid reqDto
  void $ modifyProjectReplies projectUuid (McpRepliesChangeDTO (M.singleton path (FileReply file.uuid)) [])
  return file

downloadProjectFileTool :: WizardRequestContextC s m => McpTool m
downloadProjectFileTool =
  McpTool
    { name = "download_project_file"
    , title = "Download project file"
    , description = "Get a temporary download url of a file of a project"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpTemporaryFileDTO)
    , annotations = readOnlyAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(projectUuid, fileUuid) -> toJSON <$> mcpTemporaryFile (\expiration -> downloadProjectFile expiration projectUuid fileUuid)
    }
  where
    arguments = (,) <$> projectUuidArgument <*> fileUuidArgument

deleteProjectFileTool :: WizardRequestContextC s m => McpTool m
deleteProjectFileTool =
  McpTool
    { name = "delete_project_file"
    , title = "Delete project file"
    , description = "Delete a file of a project, and with path also clear the reply that points at it; needs EDIT on the project. It cannot be undone"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(projectUuid, fileUuid, mPath) -> object [] <$ deleteFile projectUuid fileUuid mPath
    }
  where
    arguments = (,,) <$> projectUuidArgument <*> fileUuidArgument <*> optionalArgument "path" "The reply path of the file question to clear as well"

deleteFile :: WizardRequestContextC s m => U.UUID -> U.UUID -> Maybe String -> m ()
deleteFile projectUuid fileUuid mPath = do
  traverse_ (\path -> modifyProjectReplies projectUuid (McpRepliesChangeDTO M.empty [path])) mPath
  deleteProjectFile projectUuid fileUuid

fileUuidArgument :: Arguments U.UUID
fileUuidArgument = requiredArgument "file_uuid" "The file"
