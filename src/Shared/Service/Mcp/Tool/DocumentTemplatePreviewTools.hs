module Shared.Service.Mcp.Tool.DocumentTemplatePreviewTools where

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Shared.Api.Resource.Mcp.DocumentPreview.McpDocumentPreviewDTO
import Shared.Api.Resource.Mcp.DocumentPreview.McpDocumentPreviewJM ()
import Shared.Constant.Mcp
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplateDraftData
import Shared.Model.Error.Error
import Shared.Model.Localization.LocaleRecord
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.S3.Document.DocumentS3
import Shared.Service.Document.DocumentService (createDocumentPreviewForDocTmlDraft)
import Shared.Service.Document.DocumentUtil (isPendingDocument)
import Shared.Service.DocumentTemplate.DocumentTemplateAcl
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftService (modifyDraftData)
import Shared.Service.Mcp.Tool.DocumentTemplateDraftTools (draftUuidArgument)
import Shared.Service.Mcp.Tool.DocumentTemplatePreviewMapper
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWaitToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Util.JsonSchema

documentTemplatePreviewTools :: WizardRequestContextC s m => [McpTool m]
documentTemplatePreviewTools = [previewDocumentTemplate]

previewDocumentTemplate :: WizardRequestContextC s m => McpTool m
previewDocumentTemplate =
  markThirdPartyFields [["content"]] $
    McpTool
      { name = "preview_document_template"
      , title = "Preview document template"
      , description =
          "Generate a preview of a document template draft and return the rendered text for text formats, the file metadata for binary formats, "
            ++ "or the last error of the document worker with its file and line (full_log: true for the whole log). "
            ++ "Give project_uuid or knowledge_model_editor_uuid and format_uuid to change the preview source and format of the draft; without them it uses the current ones. "
            ++ "It waits a while for the document worker and then returns state pending with a documentUuid: call it again with that document_uuid to get the result. "
            ++ "Long text is cut; call it again with document_uuid and the cursor of the result to read on"
      , inputSchema = argumentsSchema previewArguments
      , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpDocumentPreviewDTO)
      , annotations = idempotentWriteAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
      , toolset = Just DtMcpToolset
      , transactionState = NoTransaction
      , run = withArguments previewArguments (fmap toJSON . preview)
      }

previewArguments :: Arguments PreviewArguments
previewArguments =
  checkArguments checkPreviewSource $
    PreviewArguments
      <$> draftUuidArgument
      <*> optionalArgument "project_uuid" "Preview over this project; it becomes the preview source of the draft"
      <*> optionalArgument "knowledge_model_editor_uuid" "Preview over this knowledge model editor instead of a project; it becomes the preview source of the draft"
      <*> optionalArgument "format_uuid" "The format to preview; it becomes the preview format of the draft"
      <*> optionalArgument "document_uuid" "The documentUuid of a pending preview, to get its result"
      <*> (fromMaybe False <$> optionalArgument "full_log" "Return the whole worker log of a failed preview, not only its last error")

checkPreviewSource :: PreviewArguments -> Either LocaleRecord PreviewArguments
checkPreviewSource arguments
  | isJust arguments.projectUuid && isJust arguments.knowledgeModelEditorUuid = Left $ _ERROR_VALIDATION__MCP_NOT_BOTH_ARGUMENTS "project_uuid" "knowledge_model_editor_uuid"
  | otherwise = Right arguments

preview :: WizardRequestContextC s m => PreviewArguments -> m McpDocumentPreviewDTO
preview arguments = do
  doc <- withShortConnection (startPreview arguments)
  waitSeconds <- asks (.serverConfig'.mcp.documentPreviewWait)
  finished <- pollWhile previewProgress waitSeconds (findDocumentByUuid doc.uuid) doc
  toPreviewResult arguments finished

startPreview :: WizardRequestContextC s m => PreviewArguments -> m Document
startPreview arguments =
  runInTransaction $ do
    checkEditorPermissionToDocumentTemplate arguments.documentTemplateUuid
    updatePreviewSettings arguments
    lockTemporaryDocumentsOfCurrentTenant
    pending <- findPendingPreviews
    (doc, _) <- createDocumentPreviewForDocTmlDraft arguments.documentTemplateUuid arguments.documentUuid
    checkPreviewLimit pending doc
    return doc

updatePreviewSettings :: WizardRequestContextC s m => PreviewArguments -> m ()
updatePreviewSettings arguments =
  when (isJust arguments.projectUuid || isJust arguments.knowledgeModelEditorUuid || isJust arguments.formatUuid) $ do
    draftData <- findDraftDataByUuid arguments.documentTemplateUuid
    void $ modifyDraftData arguments.documentTemplateUuid (toPreviewSettings arguments draftData)

toPreviewSettings :: PreviewArguments -> DocumentTemplateDraftData -> DocumentTemplateDraftDataChangeDTO
toPreviewSettings arguments draftData =
  DocumentTemplateDraftDataChangeDTO
    { projectUuid = if isJust arguments.knowledgeModelEditorUuid then Nothing else arguments.projectUuid <|> draftData.projectUuid
    , knowledgeModelEditorUuid = if isJust arguments.projectUuid then Nothing else arguments.knowledgeModelEditorUuid <|> draftData.knowledgeModelEditorUuid
    , formatUuid = arguments.formatUuid <|> draftData.formatUuid
    }

findPendingPreviews :: WizardRequestContextC s m => m [Document]
findPendingPreviews = do
  now <- liftIO getCurrentTime
  queued <- findDocumentsForCurrentTenantFiltered (previewFilter QueuedDocumentState)
  inProgress <- findDocumentsForCurrentTenantFiltered (previewFilter InProgressDocumentState)
  return $ filter ((< mcpPreviewLimitWindow) . diffUTCTime now . (.createdAt)) (queued ++ inProgress)
  where
    previewFilter state = [("durability", show TemporallyDocumentDurability), ("state", show state)]

checkPreviewLimit :: WizardRequestContextC s m => [Document] -> Document -> m ()
checkPreviewLimit pending doc = do
  limit <- asks (.serverConfig'.mcp.documentPreviewLimit)
  when (isPendingDocument doc && doc.uuid `notElem` fmap (.uuid) pending && length pending >= limit) $
    throwError . UserError $
      _ERROR_SERVICE_MCP__PREVIEW_LIMIT limit

toPreviewResult :: WizardRequestContextC s m => PreviewArguments -> Document -> m McpDocumentPreviewDTO
toPreviewResult arguments doc =
  case doc.state of
    DoneDocumentState
      | isTextContentType doc.contentType -> toTextResult doc <$> retrieveDocumentContent doc.uuid
      | otherwise -> return $ toMetadataResult doc
    ErrorDocumentState -> return $ toErrorResult arguments.fullLog doc
    _ -> return $ toPendingResult doc

data PreviewArguments = PreviewArguments
  { documentTemplateUuid :: U.UUID
  , projectUuid :: Maybe U.UUID
  , knowledgeModelEditorUuid :: Maybe U.UUID
  , formatUuid :: Maybe U.UUID
  , documentUuid :: Maybe U.UUID
  , fullLog :: Bool
  }
