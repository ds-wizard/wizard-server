module Shared.Service.Mcp.Tool.DocumentTemplateFileTools where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM ()
import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListJM ()
import Shared.Api.Resource.Mcp.DocumentTemplate.McpAssetWriteDTO
import Shared.Api.Resource.Mcp.DocumentTemplate.McpAssetWriteJM ()
import Shared.Api.Resource.Mcp.DocumentTemplate.McpFileWriteDTO
import Shared.Api.Resource.Mcp.DocumentTemplate.McpFileWriteJM ()
import Shared.Constant.Mcp
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Shared.Model.Localization.LocaleRecord
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetService
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileBatchService
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileService
import Shared.Service.Mcp.Tool.DocumentTemplateDraftTools (draftUuidArgument)
import Shared.Service.Mcp.Tool.DocumentTemplatePreviewMapper (isTextContentType)
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Util.JsonSchema

documentTemplateFileTools :: WizardRequestContextC s m => [McpTool m]
documentTemplateFileTools =
  [readDocumentTemplateFiles, writeDocumentTemplateFiles, readDocumentTemplateAssets, writeDocumentTemplateAssets, deleteDocumentTemplateFiles]

readDocumentTemplateFiles :: forall s m. WizardRequestContextC s m => McpTool m
readDocumentTemplateFiles =
  markThirdPartyFields [["files", "content"]] $
    McpTool
      { name = "read_document_template_files"
      , title = "Read document template files"
      , description = "Read the content of several files of a document template by their paths; paths that name no file (for example assets) are returned as missing"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just readFilesSchema
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
      , toolset = Just DtMcpToolset
      , transactionState = NoTransaction
      , run = withArguments arguments (uncurry readFiles)
      }
  where
    arguments = (,) <$> draftUuidArgument <*> requiredArgument "paths" "The paths of the files, for example src/default.html.j2" :: Arguments (U.UUID, [String])

readFiles :: WizardRequestContextC s m => U.UUID -> [String] -> m Value
readFiles dtUuid paths = do
  files <- getTemplateFilesByPaths dtUuid paths
  let missing = filter (\path -> not (any ((== path) . (.fileName)) files)) paths
  return $ object ["files" .= fmap fileContent files, "missing" .= missing]

fileContent :: DocumentTemplateFile -> Value
fileContent file = object ["path" .= file.fileName, "content" .= file.content]

readFilesSchema :: Value
readFilesSchema =
  objectSchema
    ["files", "missing"]
    [ ("files", arraySchema (objectSchema ["path", "content"] [("path", stringSchema), ("content", stringSchema)]))
    , ("missing", describedSchema "The paths that name no file" (arraySchema stringSchema))
    ]

writeDocumentTemplateFiles :: forall s m. WizardRequestContextC s m => McpTool m
writeDocumentTemplateFiles =
  McpTool
    { name = "write_document_template_files"
    , title = "Write document template files"
    , description =
        "Create, change and rename files of a document template draft, and move folders, in one batch that is stored completely or not at all. "
          ++ "An entry with content creates the file at path or replaces its content; with new_path it renames the file. A path ending with / is a folder: "
          ++ "new_path moves it with everything in it. The content of a file is limited to "
          ++ show mcpMaxFileContentLength
          ++ " characters and of a batch to "
          ++ show mcpMaxFilesContentLength
          ++ ". It returns the files and assets of the draft afterwards"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just filesSchema
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments (uncurry writeFiles)
    }
  where
    arguments = (,) <$> draftUuidArgument <*> checkArguments checkContentLength (requiredArgument "files" "The files to write, in order") :: Arguments (U.UUID, [McpFileWriteDTO])

checkContentLength :: [McpFileWriteDTO] -> Either LocaleRecord [McpFileWriteDTO]
checkContentLength writes
  | Just write <- find ((> mcpMaxFileContentLength) . contentLength) writes =
      Left $ _ERROR_VALIDATION__MCP_FILE_CONTENT_LENGTH write.path mcpMaxFileContentLength
  | sum (fmap contentLength writes) > mcpMaxFilesContentLength =
      Left $ _ERROR_VALIDATION__MCP_FILES_CONTENT_LENGTH mcpMaxFilesContentLength
  | otherwise = Right writes
  where
    contentLength = maybe 0 length . (.content)

writeFiles :: WizardRequestContextC s m => U.UUID -> [McpFileWriteDTO] -> m Value
writeFiles dtUuid writes = do
  writeDraftFiles dtUuid writes
  getFilesAndAssets dtUuid

readDocumentTemplateAssets :: forall s m. WizardRequestContextC s m => McpTool m
readDocumentTemplateAssets =
  markThirdPartyFields [["assets", "content"]] $
    McpTool
      { name = "read_document_template_assets"
      , title = "Read document template assets"
      , description =
          "Read assets of a document template by their paths: each comes with a download url valid until its urlExpiration (15 minutes), and a text asset (such as CSS, JSON or SVG) also with its content. "
            ++ "Paths that name no asset are returned as missing"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just readAssetsSchema
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
      , toolset = Just DtMcpToolset
      , transactionState = NoTransaction
      , run = withArguments arguments (uncurry readAssets)
      }
  where
    arguments = (,) <$> draftUuidArgument <*> requiredArgument "paths" "The paths of the assets, for example src/logo.png" :: Arguments (U.UUID, [String])

readAssets :: WizardRequestContextC s m => U.UUID -> [String] -> m Value
readAssets dtUuid paths = do
  assets <- filter ((`elem` paths) . (.fileName)) <$> getAssets mcpTemporaryFileExpiration dtUuid
  contents <- traverse (assetContent dtUuid) assets
  let missing = filter (\path -> not (any ((== path) . (.fileName)) assets)) paths
  return $ object ["assets" .= contents, "missing" .= missing]

assetContent :: WizardRequestContextC s m => U.UUID -> DocumentTemplateAssetDTO -> m Value
assetContent dtUuid asset
  | isTextContentType (Just asset.contentType) = do
      (_, content) <- getAssetContent dtUuid asset.uuid
      return $ withField "content" (decodeText content) (toJSON asset)
  | otherwise = return $ toJSON asset

withField :: String -> String -> Value -> Value
withField name value (Object o) = Object (KM.insert (K.fromString name) (toJSON value) o)
withField _ _ value = value

readAssetsSchema :: Value
readAssetsSchema =
  objectSchema
    ["assets", "missing"]
    [ ("assets", arraySchema (withProperties [("content", describedSchema "The content of a text asset" stringSchema)] (jsonSchema (Proxy :: Proxy DocumentTemplateAssetDTO))))
    , ("missing", describedSchema "The paths that name no asset" (arraySchema stringSchema))
    ]

writeDocumentTemplateAssets :: forall s m. WizardRequestContextC s m => McpTool m
writeDocumentTemplateAssets =
  McpTool
    { name = "write_document_template_assets"
    , title = "Write document template assets"
    , description =
        "Upload, replace and rename assets (images, fonts and other binary files) of a document template draft in one batch that is checked completely before anything is written. "
          ++ "An entry with base64 content creates the asset at path (content_type is then needed) or replaces its content; with new_path it renames the asset, which is refused when another asset or file has that path. "
          ++ "The decoded content of a batch is limited to 10 MB. It returns the assets of the draft afterwards"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ objectSchema ["assets"] [("assets", arraySchema (jsonSchema (Proxy :: Proxy DocumentTemplateAssetDTO)))]
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments (uncurry writeAssets)
    }
  where
    arguments = (,) <$> draftUuidArgument <*> checkArguments decodeAssetWrites (requiredArgument "assets" "The assets to write, in order") :: Arguments (U.UUID, [McpAssetWriteDTO])

decodeAssetWrites :: [McpAssetWriteDTO] -> Either LocaleRecord [McpAssetWriteDTO]
decodeAssetWrites writes = do
  decoded <- traverse decodeAssetWrite writes
  if sum (fmap (maybe 0 BS.length . (.content)) decoded) > mcpMaxBase64ContentSize
    then Left $ _ERROR_VALIDATION__MCP_BASE64_SIZE "assets" mcpMaxBase64ContentSize
    else Right decoded

decodeAssetWrite :: McpAssetWriteDTO -> Either LocaleRecord McpAssetWriteDTO
decodeAssetWrite write = (\content -> McpAssetWriteDTO write.path content write.contentType write.newPath) <$> traverse (decodeBase64Content "content") write.content

writeAssets :: WizardRequestContextC s m => U.UUID -> [McpAssetWriteDTO] -> m Value
writeAssets dtUuid writes = do
  writeDraftAssets dtUuid writes
  (\assets -> object ["assets" .= assets]) <$> getAssets mcpTemporaryFileExpiration dtUuid

deleteDocumentTemplateFiles :: forall s m. WizardRequestContextC s m => McpTool m
deleteDocumentTemplateFiles =
  McpTool
    { name = "delete_document_template_files"
    , title = "Delete document template files"
    , description =
        "Delete files and assets of a document template draft by their paths in one batch; a path ending with / deletes the folder with everything in it. "
          ++ "Paths that name nothing are skipped. It returns the deleted paths and cannot be undone"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ objectSchema ["deleted"] [("deleted", arraySchema stringSchema)]
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments (uncurry deletePaths)
    }
  where
    arguments = (,) <$> draftUuidArgument <*> requiredArgument "paths" "The paths of the files and assets, or of folders ending with /" :: Arguments (U.UUID, [String])

deletePaths :: WizardRequestContextC s m => U.UUID -> [String] -> m Value
deletePaths dtUuid paths = (\deleted -> object ["deleted" .= deleted]) <$> deleteDraftPaths dtUuid paths

getFilesAndAssets :: WizardRequestContextC s m => U.UUID -> m Value
getFilesAndAssets dtUuid = do
  files <- getFiles dtUuid
  assets <- getAssets mcpTemporaryFileExpiration dtUuid
  return $ object ["files" .= files, "assets" .= assets]

filesSchema :: Value
filesSchema =
  objectSchema
    ["files", "assets"]
    [ ("files", arraySchema (jsonSchema (Proxy :: Proxy DocumentTemplateFileList)))
    , ("assets", arraySchema (jsonSchema (Proxy :: Proxy DocumentTemplateAssetDTO)))
    ]
