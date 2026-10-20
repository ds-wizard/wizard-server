module Shared.Service.Mcp.Tool.DocumentTemplatePublishTools where

import Data.Aeson (toJSON)
import Data.List (intercalate)
import Data.Proxy
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftPublication
import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.DocumentTemplate.DocumentTemplateService (getDocumentTemplateByUuidDto)
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftService (getDraftPublication, publishDraft)
import Shared.Service.Mcp.Tool.DocumentTemplateDraftTools (draftUuidArgument)
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpToolCommon (argumentsSchema)
import Shared.Util.JsonSchema

documentTemplatePublishTools :: WizardRequestContextC s m => [McpTool m]
documentTemplatePublishTools = [publishDocumentTemplateDraft]

publishDocumentTemplateDraft :: WizardRequestContextC s m => McpTool m
publishDocumentTemplateDraft =
  McpTool
    { name = "publish_document_template_draft"
    , title = "Publish document template draft"
    , description =
        "Publish a document template draft as a released template with its name, id, version, formats, files and assets "
          ++ "(change the version with save_document_template_draft first); the version must be higher than the latest released one of the id. "
          ++ "Publishing cannot be undone. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments draftUuidArgument)
    , outputSchema = Just $ confirmationOutputSchema (jsonSchema (Proxy :: Proxy DocumentTemplateDetailDTO))
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "publish_document_template_draft" draftUuidArgument preparePublish
    }

preparePublish :: WizardRequestContextC s m => U.UUID -> m (McpConfirmation m)
preparePublish dtUuid = do
  publication <- getDraftPublication dtUuid
  return
    McpConfirmation
      { summary = publishSummary publication
      , revision = intercalate "/" (iso8601Show publication.draft.updatedAt : fmap fileRevision publication.files ++ fmap assetRevision publication.assets)
      , perform = toJSON <$> publish publication.draft
      }

publish :: WizardRequestContextC s m => DocumentTemplate -> m DocumentTemplateDetailDTO
publish draft = publishDraft draft >> getDocumentTemplateByUuidDto draft.uuid

publishSummary :: DocumentTemplateDraftPublication -> String
publishSummary DocumentTemplateDraftPublication {..} =
  unwords
    [ "Publishes the document template draft " ++ show draft.name ++ " as the released template " ++ draft.id ++ ":" ++ draft.version
    , "with " ++ show (length formats) ++ " formats, " ++ show (length files) ++ " files and " ++ show (length assets) ++ " assets."
    , maybe "It is the first released version of the id." (\latest -> "The latest released version is " ++ latest.version ++ ".") latestRelease
    , "The released template cannot be changed afterwards and publishing cannot be undone."
    ]

fileRevision :: DocumentTemplateFileList -> String
fileRevision file = U.toString file.uuid ++ "@" ++ iso8601Show file.updatedAt

assetRevision :: DocumentTemplateAsset -> String
assetRevision asset = U.toString asset.uuid ++ "@" ++ iso8601Show asset.updatedAt
