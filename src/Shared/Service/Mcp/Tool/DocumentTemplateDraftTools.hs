module Shared.Service.Mcp.Tool.DocumentTemplateDraftTools where

import Control.Monad (void)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Proxy
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListJM ()
import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListJM ()
import Shared.Constant.Mcp
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Shared.Model.DocumentTemplate.DocumentTemplateDraftList
import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetService
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftService
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileService
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Util.JsonSchema

documentTemplateDraftTools :: WizardRequestContextC s m => [McpTool m]
documentTemplateDraftTools = [listDocumentTemplateDrafts, getDocumentTemplateDraft, saveDocumentTemplateDraft, deleteDocumentTemplateDraft]

listDocumentTemplateDrafts :: forall s m. WizardRequestContextC s m => McpTool m
listDocumentTemplateDrafts =
  McpTool
    { name = "list_document_template_drafts"
    , title = "List document template drafts"
    , description = "List the document template drafts you can work on, optionally in one workspace (or at the organization level with tenant: true) and filtered by name or id"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "documentTemplateDrafts" (jsonSchema (Proxy :: Proxy DocumentTemplateDraftList))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(inScope, (mQuery, pageable)) -> inScope (toJSON <$> getDraftsPage mQuery pageable [Sort "name" Ascending])
    }
  where
    arguments = (,) <$> scopeArguments <*> searchArguments "Search in the names and ids of the drafts" :: Arguments (m Value -> m Value, (Maybe String, Pageable))

getDocumentTemplateDraft :: WizardRequestContextC s m => McpTool m
getDocumentTemplateDraft =
  markThirdPartyFields [["readme"]] $
    McpTool
      { name = "get_document_template_draft"
      , title = "Get document template draft"
      , description =
          "Get a document template draft: its metadata, its formats with their steps, its preview settings (projectUuid or knowledgeModelEditorUuid, and formatUuid) "
            ++ "and the paths of its files and assets. Read the content of the files with read_document_template_files"
      , inputSchema = argumentsSchema draftUuidArgument
      , outputSchema = Just draftWithFilesSchema
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
      , toolset = Just DtMcpToolset
      , transactionState = NoTransaction
      , run = withArguments draftUuidArgument getDraftWithFiles
      }

getDraftWithFiles :: WizardRequestContextC s m => U.UUID -> m Value
getDraftWithFiles dtUuid = do
  draft <- getDraft dtUuid
  files <- getFiles dtUuid
  assets <- getAssets mcpTemporaryFileExpiration dtUuid
  return $ withFields [("files", toJSON files), ("assets", toJSON assets)] (toJSON draft)

withFields :: [(Key, Value)] -> Value -> Value
withFields fields (Object o) = Object (KM.union (KM.fromList fields) o)
withFields _ value = value

draftWithFilesSchema :: Value
draftWithFilesSchema =
  withRequired ["files", "assets"] . withProperties fileProperties $ jsonSchema (Proxy :: Proxy DocumentTemplateDraftDetail)
  where
    fileProperties =
      [ ("files", arraySchema (jsonSchema (Proxy :: Proxy DocumentTemplateFileList)))
      , ("assets", arraySchema (jsonSchema (Proxy :: Proxy DocumentTemplateAssetDTO)))
      ]

saveDocumentTemplateDraft :: forall s m. WizardRequestContextC s m => McpTool m
saveDocumentTemplateDraft =
  markThirdPartyFields [["readme"]] $
    McpTool
      { name = "save_document_template_draft"
      , title = "Save document template draft"
      , description =
          "Create a document template draft or change its metadata, allowed packages and formats; only the fields you give change, and formats, when given, replace all formats. "
            ++ "A new draft needs name, id and version and belongs to a workspace, or to the organization with tenant: true; with based_on it starts as a copy of that template "
            ++ "(its metadata, formats, files and assets) in the current metamodel. "
            ++ uuidDescription "a draft"
            ++ ". "
            ++ expectedUpdatedAtDescription "draft"
            ++ ". It does not publish the draft"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just $ jsonSchema (Proxy :: Proxy DocumentTemplateDraftDetail)
      , annotations = writeAnnotations
      , continuable = False
      , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
      , toolset = Just DtMcpToolset
      , transactionState = Transactional
      , run = withArguments arguments $ \(inScope, mUuid, mExpectedUpdatedAt, mBasedOn, changes) ->
          inScope (toJSON <$> saveDraft mUuid mExpectedUpdatedAt mBasedOn changes)
      }
  where
    arguments =
      (,,,,)
        <$> scopeArguments
        <*> uuidArgument draftEntityName
        <*> expectedUpdatedAtArgument
        <*> optionalArgument "based_on" "For a new draft, the document template to copy; ignored for an existing draft"
        <*> changesArguments (withoutProperties ["phase"] (jsonSchema (Proxy :: Proxy DocumentTemplateDraftChangeDTO)))
        :: Arguments (m Value -> m Value, Maybe U.UUID, Maybe UTCTime, Maybe U.UUID, Object)

saveDraft :: WizardRequestContextC s m => Maybe U.UUID -> Maybe UTCTime -> Maybe U.UUID -> Object -> m DocumentTemplateDraftDetail
saveDraft mUuid mExpectedUpdatedAt mBasedOn changes =
  runInTransaction $
    saveByUuid
      findDraftToChange
      (createDraftFromChanges mBasedOn changes)
      (updateDraftFromChanges mExpectedUpdatedAt changes)
      mUuid

createDraftFromChanges :: WizardRequestContextC s m => Maybe U.UUID -> Object -> U.UUID -> m DocumentTemplateDraftDetail
createDraftFromChanges mBasedOn changes uuid = do
  _ <- createFromChanges draftEntityName ["name", "id", "version"] (parseChanges . KM.insert "basedOn" (toJSON mBasedOn)) (createDraftWithUuid uuid) changes
  updateDraftFromChanges Nothing changes =<< getDraftToChange uuid

updateDraftFromChanges :: WizardRequestContextC s m => Maybe UTCTime -> Object -> DocumentTemplate -> m DocumentTemplateDraftDetail
updateDraftFromChanges mExpectedUpdatedAt changes dt = do
  current <- getDraftChangeDTO dt
  changed <- mergeChangesSince draftEntityName mExpectedUpdatedAt dt.updatedAt changes current
  modifyIfChanged current changed (return ()) (void . modifyDraft dt.uuid)
  getDraft dt.uuid

deleteDocumentTemplateDraft :: WizardRequestContextC s m => McpTool m
deleteDocumentTemplateDraft =
  McpTool
    { name = "delete_document_template_draft"
    , title = "Delete document template draft"
    , description = "Delete a document template draft with its files and assets; released templates are not affected. It cannot be undone"
    , inputSchema = argumentsSchema draftUuidArgument
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = Transactional
    , run = withArguments draftUuidArgument $ \dtUuid -> object [] <$ deleteDraft dtUuid
    }

draftUuidArgument :: Arguments U.UUID
draftUuidArgument = requiredArgument "document_template_uuid" "The document template draft"

draftEntityName :: String
draftEntityName = "document template draft"
