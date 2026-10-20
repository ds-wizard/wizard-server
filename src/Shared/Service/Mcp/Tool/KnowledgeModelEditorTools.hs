module Shared.Service.Mcp.Tool.KnowledgeModelEditorTools where

import Control.Monad (unless, void)
import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Proxy
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorMetadataDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorMetadataJM ()
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Editor.EditorMapper
import Shared.Service.KnowledgeModel.Editor.EditorService
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Util.JsonSchema

knowledgeModelEditorTools :: WizardRequestContextC s m => [McpTool m]
knowledgeModelEditorTools = [listKnowledgeModelEditors, saveKnowledgeModelEditor, deleteKnowledgeModelEditor]

listKnowledgeModelEditors :: forall s m. WizardRequestContextC s m => McpTool m
listKnowledgeModelEditors =
  McpTool
    { name = "list_knowledge_model_editors"
    , title = "List knowledge model editors"
    , description =
        "List the knowledge model editors you can work in, optionally in one workspace and filtered by name or id. "
          ++ "The state tells whether the editor has unpublished changes (EditedKnowledgeModelEditorState), "
          ++ "is based on an outdated package version (OutdatedKnowledgeModelEditorState) or is in a migration"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "knowledgeModelEditors" (jsonSchema (Proxy :: Proxy KnowledgeModelEditorList))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION, _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(inScope, (mQuery, pageable)) -> inScope (toJSON <$> getEditorsPage mQuery pageable [])
    }
  where
    arguments = (,) <$> scopeArguments <*> searchArguments "Search in the names and ids of the editors" :: Arguments (m Value -> m Value, (Maybe String, Pageable))

saveKnowledgeModelEditor :: forall s m. WizardRequestContextC s m => McpTool m
saveKnowledgeModelEditor =
  markThirdPartyFields [["readme"]] $
    McpTool
      { name = "save_knowledge_model_editor"
      , title = "Save knowledge model editor"
      , description =
          "Create a knowledge model editor or change its publication metadata; only the fields you give change. "
            ++ "A new editor needs name, id and version and is created in a workspace: with previous_package_uuid it continues that package (same id) "
            ++ "or forks it (another id), without it starts a new knowledge model. "
            ++ uuidDescription "an editor"
            ++ ". "
            ++ expectedUpdatedAtDescription "editor"
            ++ "; a call with only uuid returns its current state"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just $ jsonSchema (Proxy :: Proxy KnowledgeModelEditorMetadataDTO)
      , annotations = writeAnnotations
      , continuable = False
      , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION]
      , toolset = Just KmMcpToolset
      , transactionState = Transactional
      , run = withArguments arguments $ \(inScope, mUuid, mExpectedUpdatedAt, mPreviousPackageUuid, changes) ->
          inScope (toJSON <$> saveEditor mUuid mExpectedUpdatedAt mPreviousPackageUuid changes)
      }
  where
    arguments =
      (,,,,)
        <$> scopeArguments
        <*> uuidArgument editorEntityName
        <*> expectedUpdatedAtArgument
        <*> optionalArgument "previous_package_uuid" "For a new editor, the package it continues or forks"
        <*> changesArguments (jsonSchema (Proxy :: Proxy KnowledgeModelEditorChangeDTO))
        :: Arguments (m Value -> m Value, Maybe U.UUID, Maybe UTCTime, Maybe U.UUID, Object)

saveEditor :: WizardRequestContextC s m => Maybe U.UUID -> Maybe UTCTime -> Maybe U.UUID -> Object -> m KnowledgeModelEditorMetadataDTO
saveEditor mUuid mExpectedUpdatedAt mPreviousPackageUuid changes =
  runInTransaction $
    saveByUuid
      findEditorWithPermission
      (createEditorFromChanges mPreviousPackageUuid changes)
      (updateEditorFromChanges mExpectedUpdatedAt mPreviousPackageUuid changes)
      mUuid

createEditorFromChanges :: WizardRequestContextC s m => Maybe U.UUID -> Object -> U.UUID -> m KnowledgeModelEditorMetadataDTO
createEditorFromChanges mPreviousPackageUuid changes uuid = do
  let parse = parseChanges . KM.insert "previousPackageUuid" (toJSON mPreviousPackageUuid)
  _ <- createFromChanges editorEntityName ["name", "id", "version"] parse (createEditorWithUuid uuid) changes
  updateEditorFromChanges Nothing mPreviousPackageUuid changes =<< getEditorWithPermission uuid

updateEditorFromChanges :: WizardRequestContextC s m => Maybe UTCTime -> Maybe U.UUID -> Object -> KnowledgeModelEditor -> m KnowledgeModelEditorMetadataDTO
updateEditorFromChanges mExpectedUpdatedAt mPreviousPackageUuid changes editor = do
  checkUnchangedSince editorEntityName mExpectedUpdatedAt editor.updatedAt
  unless (maybe True ((== editor.previousPackageUuid) . Just) mPreviousPackageUuid) $
    throwError . UserError $
      _ERROR_SERVICE_MCP__UNCHANGEABLE_ARGUMENT "previous_package_uuid" editorEntityName
  let current = toChangeDTO editor
  changed <- mergeChanges changes current
  modifyIfChanged current changed (return ()) (void . modifyEditor editor.uuid)
  toMetadataDTO <$> getEditorWithPermission editor.uuid

deleteKnowledgeModelEditor :: WizardRequestContextC s m => McpTool m
deleteKnowledgeModelEditor =
  McpTool
    { name = "delete_knowledge_model_editor"
    , title = "Delete knowledge model editor"
    , description = "Delete a knowledge model editor with all its unpublished changes; the packages published from it stay. It cannot be undone"
    , inputSchema = argumentsSchema editorUuidArgument
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withArguments editorUuidArgument $ \editorUuid -> object [] <$ deleteEditor editorUuid
    }

editorUuidArgument :: Arguments U.UUID
editorUuidArgument = requiredArgument "knowledge_model_editor_uuid" "The knowledge model editor"

editorEntityName :: String
editorEntityName = "knowledge model editor"
