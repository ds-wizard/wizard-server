module Shared.Service.DocumentTemplate.File.DocumentTemplateFileBatchService where

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, void)
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO
import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO
import Shared.Api.Resource.Mcp.DocumentTemplate.McpAssetWriteDTO
import Shared.Api.Resource.Mcp.DocumentTemplate.McpFileWriteDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Shared.Model.Error.Error
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetService
import Shared.Service.DocumentTemplate.DocumentTemplateAuditLog
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftService
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileService
import Shared.Service.DocumentTemplate.Folder.DocumentTemplateFolderService
import Shared.Service.Tenant.Limit.WizardLimitService

writeDraftFiles :: WizardRequestContextC s m => U.UUID -> [McpFileWriteDTO] -> m ()
writeDraftFiles dtUuid writes =
  runInTransaction $ do
    dt <- getDraftToChange dtUuid
    traverse_ (writeFileEntry dtUuid) writes
    al_writeDocumentTemplateFiles dt (fmap (.path) writes)

writeFileEntry :: WizardRequestContextC s m => U.UUID -> McpFileWriteDTO -> m ()
writeFileEntry dtUuid write
  | isFolderPath write.path = moveFolderEntry dtUuid write
  | otherwise = do
      mFile <- findFileByPath dtUuid write.path
      mRenamed <- maybe (return Nothing) (findFileByPath dtUuid) write.newPath
      case (mFile <|> mRenamed, write.content) of
        (Just file, mContent) -> void $ modifyFile file.uuid (DocumentTemplateFileChangeDTO (fromMaybe file.fileName write.newPath) (fromMaybe file.content mContent))
        (Nothing, Just content) -> void $ createFile dtUuid (DocumentTemplateFileChangeDTO (fromMaybe write.path write.newPath) content)
        (Nothing, Nothing) -> throwError . UserError $ _ERROR_SERVICE_MCP__FILE_NOT_FOUND write.path

moveFolderEntry :: WizardRequestContextC s m => U.UUID -> McpFileWriteDTO -> m ()
moveFolderEntry dtUuid write =
  case (write.newPath, write.content) of
    (Just newPath, Nothing) -> moveDraftFolder dtUuid (DocumentTemplateFolderMoveDTO write.path newPath)
    _ -> throwError . UserError $ _ERROR_SERVICE_MCP__FOLDER_WRITE write.path

findFileByPath :: WizardRequestContextC s m => U.UUID -> String -> m (Maybe DocumentTemplateFile)
findFileByPath dtUuid path = listToMaybe <$> findFilesByDocumentTemplateUuidAndFileName dtUuid path

writeDraftAssets :: WizardRequestContextC s m => U.UUID -> [McpAssetWriteDTO] -> m ()
writeDraftAssets dtUuid writes =
  runInTransaction $ do
    dt <- getDraftToChange dtUuid
    checkAssetWrites dtUuid writes
    traverse_ (writeAssetEntry dtUuid) writes
    al_writeDocumentTemplateFiles dt (fmap (.path) writes)

writeAssetEntry :: WizardRequestContextC s m => U.UUID -> McpAssetWriteDTO -> m ()
writeAssetEntry dtUuid write = do
  mAsset <- findAssetByPath dtUuid write.path
  mRenamed <- maybe (return Nothing) (findAssetByPath dtUuid) write.newPath
  case (mAsset <|> mRenamed, write.content, write.contentType) of
    (Just asset, Just content, mContentType) ->
      void $ modifyAssetContent asset.uuid (DocumentTemplateAssetCreateDTO (fromMaybe asset.fileName write.newPath) (fromMaybe asset.contentType mContentType) content)
    (Just asset, Nothing, _) -> void $ modifyAsset asset.uuid (DocumentTemplateAssetChangeDTO (fromMaybe asset.fileName write.newPath))
    (Nothing, Just content, Just contentType) -> void $ createAsset dtUuid (DocumentTemplateAssetCreateDTO (fromMaybe write.path write.newPath) contentType content)
    _ -> throwError . UserError $ _ERROR_SERVICE_MCP__ASSET_NOT_FOUND write.path

checkAssetWrites :: WizardRequestContextC s m => U.UUID -> [McpAssetWriteDTO] -> m ()
checkAssetWrites dtUuid writes = do
  assetNames <- fmap (.fileName) <$> findAssetsByDocumentTemplateUuid dtUuid
  fileNames <- fmap (.fileName) <$> findFileListsByDocumentTemplateUuid dtUuid
  either throwError (const (return ())) (foldM (checkAssetWrite fileNames) assetNames writes)
  checkStorageSize (sum (fmap (maybe 0 (fromIntegral . BS.length) . (.content)) writes))

checkAssetWrite :: [String] -> [String] -> McpAssetWriteDTO -> Either AppError [String]
checkAssetWrite fileNames names write
  | exists && renamed && taken target = Left (UserError _ERROR_VALIDATION__DOC_TML_FILE_OR_ASSET_UNIQUENESS)
  | exists = Right (target : filter (/= write.path) names)
  | maybe False (`elem` names) write.newPath = Right names
  | isNothing write.content || isNothing write.contentType = Left (UserError $ _ERROR_SERVICE_MCP__ASSET_NOT_FOUND write.path)
  | taken target = Left (UserError _ERROR_VALIDATION__DOC_TML_FILE_OR_ASSET_UNIQUENESS)
  | otherwise = Right (target : names)
  where
    exists = write.path `elem` names
    target = fromMaybe write.path write.newPath
    renamed = target /= write.path
    taken name = name `elem` names || name `elem` fileNames

findAssetByPath :: WizardRequestContextC s m => U.UUID -> String -> m (Maybe DocumentTemplateAsset)
findAssetByPath dtUuid path = find ((== path) . (.fileName)) <$> findAssetsByDocumentTemplateUuid dtUuid

deleteDraftPaths :: WizardRequestContextC s m => U.UUID -> [String] -> m [String]
deleteDraftPaths dtUuid paths =
  runInTransaction $ do
    dt <- getDraftToChange dtUuid
    files <- filter (matchesAnyPath paths . (.fileName)) <$> findFileListsByDocumentTemplateUuid dtUuid
    assets <- filter (matchesAnyPath paths . (.fileName)) <$> findAssetsByDocumentTemplateUuid dtUuid
    traverse_ (deleteFile . (.uuid)) files
    traverse_ (deleteAsset dtUuid . (.uuid)) assets
    let deleted = fmap (.fileName) files ++ fmap (.fileName) assets
    unless (null deleted) (al_deleteDocumentTemplateFiles dt deleted)
    return deleted

matchesAnyPath :: [String] -> String -> Bool
matchesAnyPath paths name = any (`matchesPath` name) paths

matchesPath :: String -> String -> Bool
matchesPath path name
  | isFolderPath path = path `isPrefixOf` name
  | otherwise = path == name

isFolderPath :: String -> Bool
isFolderPath = isSuffixOf "/"
