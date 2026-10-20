module Shared.Service.Mcp.Tool.DocumentTemplateLibraryTools where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListJM ()
import Shared.Api.Resource.DocumentTemplate.WizardDocumentTemplateSimpleJM ()
import Shared.Api.Resource.Library.LibraryDependentsJM ()
import Shared.Api.Resource.Mcp.TemporaryFile.McpTemporaryFileDTO
import Shared.Api.Resource.Mcp.TemporaryFile.McpTemporaryFileJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Model.Error.Error
import Shared.Model.Library.LibraryDependents
import Shared.Model.Localization.LocaleRecord
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.Acl.LibraryAcl (checkDeleteAllowed)
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService (getTemporaryFileWithBundle, importAndConvertBundle, pullBundleFromRegistry)
import Shared.Service.DocumentTemplate.DocumentTemplateService
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileService (getTemplateFiles)
import Shared.Service.Mcp.Tool.KnowledgeModelPackageTools (allVersionsArgument, countsSummary, deletedVersions)
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Util.Error (tryError)
import Shared.Util.JsonSchema
import Shared.Util.Reference (parseReference)

documentTemplateLibraryTools :: WizardRequestContextC s m => [McpTool m]
documentTemplateLibraryTools =
  [ listDocumentTemplates
  , getDocumentTemplate
  , pullDocumentTemplate
  , updateDocumentTemplate
  , deleteDocumentTemplateTool
  , exportDocumentTemplate
  , importDocumentTemplate
  ]

listDocumentTemplates :: forall s m. WizardRequestContextC s m => McpTool m
listDocumentTemplates =
  McpTool
    { name = "list_document_templates"
    , title = "List document templates"
    , description =
        "List the released and deprecated document templates (the latest version of each id), optionally in one workspace, of one id or only those with a newer version in the registry. "
          ++ "remoteLatestVersion is the latest version in the registry. To change a template, create a draft based on it with save_document_template_draft"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "documentTemplates" (jsonSchema (Proxy :: Proxy DocumentTemplateSimpleDTO))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(inScope, mId, mOutdated, (mQuery, pageable)) ->
        inScope (toJSON <$> getDocumentTemplatesPage mId mQuery mOutdated pageable [Sort "name" Ascending])
    }
  where
    arguments =
      (,,,)
        <$> scopeArguments
        <*> optionalArgument "id" "Only the template with this id (without the version)"
        <*> optionalArgument "outdated" "Only the templates with (true) or without (false) a newer version in the registry"
        <*> searchArguments "Search in the names and ids of the templates"
        :: Arguments (m Value -> m Value, Maybe String, Maybe Bool, (Maybe String, Pageable))

getDocumentTemplate :: WizardRequestContextC s m => McpTool m
getDocumentTemplate =
  markThirdPartyFields [["documentTemplate", "readme"]] $
    McpTool
      { name = "get_document_template"
      , title = "Get document template"
      , description =
          "Get a document template with its formats, versions, locales, the knowledge models it can be used with and its registry state; "
            ++ "when you may manage it its dependents (projects and documents using it, check them before deprecating it), "
            ++ "and when you may edit templates the paths of its files (read them with read_document_template_files)"
      , inputSchema = argumentsSchema templateUuidArgument
      , outputSchema =
          Just $
            objectSchema
              ["documentTemplate", "dependents", "files"]
              [ ("documentTemplate", jsonSchema (Proxy :: Proxy DocumentTemplateDetailDTO))
              , ("dependents", describedSchema "Left out without documentTemplates.manage" (jsonSchema (Proxy :: Proxy (Maybe LibraryDependents))))
              , ("files", describedSchema "Left out without documentTemplates.useEditor" (jsonSchema (Proxy :: Proxy (Maybe [DocumentTemplateFileList]))))
              ]
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just DtMcpToolset
      , transactionState = NoTransaction
      , run = withArguments templateUuidArgument getTemplateWithDependents
      }

getTemplateWithDependents :: WizardRequestContextC s m => U.UUID -> m Value
getTemplateWithDependents dtUuid = do
  detail <- getDocumentTemplateByUuidDto dtUuid
  mDependents <- whenPermitted (getDocumentTemplateDependents dtUuid Nothing)
  mFiles <- whenPermitted (getTemplateFiles dtUuid)
  return $ object ["documentTemplate" .= detail, "dependents" .= mDependents, "files" .= mFiles]

whenPermitted :: WizardRequestContextC s m => m a -> m (Maybe a)
whenPermitted action = do
  eResult <- tryError action
  case eResult of
    Right result -> return (Just result)
    Left (ForbiddenError _) -> return Nothing
    Left error -> throwError error

pullDocumentTemplate :: forall s m. WizardRequestContextC s m => McpTool m
pullDocumentTemplate =
  McpTool
    { name = "pull_document_template"
    , title = "Pull document template"
    , description =
        "Pull a document template from the registry into a workspace (or the organization with tenant: true). "
          ++ "id is the full id with the version, as search_registry_packages returns it plus \":\" and the version"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy DocumentTemplateSimple)
    , annotations = idempotentWriteAnnotations {openWorldHint = True}
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(inScope, coordinate) -> inScope (toJSON <$> pullBundleFromRegistry coordinate)
    }
  where
    arguments =
      (,)
        <$> scopeArguments
        <*> checkArguments parseCoordinate (requiredArgument "id" "The template to pull, e.g. dsw:questionnaire-report:2.12.0")
        :: Arguments (m Value -> m Value, Coordinate)

parseCoordinate :: String -> Either LocaleRecord Coordinate
parseCoordinate reference = maybe (Left $ _ERROR_VALIDATION__MCP_COORDINATE reference) Right (parseReference reference)

updateDocumentTemplate :: WizardRequestContextC s m => McpTool m
updateDocumentTemplate =
  McpTool
    { name = "update_document_template"
    , title = "Update document template"
    , description = "Deprecate or release again a released document template version. Check its dependents with get_document_template first; publish a draft with publish_document_template_draft"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy DocumentTemplateDetailDTO)
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(dtUuid, phase) -> toJSON <$> updatePhase dtUuid phase
    }
  where
    arguments =
      (,)
        <$> templateUuidArgument
        <*> requiredArgument "phase" "ReleasedDocumentTemplatePhase or DeprecatedDocumentTemplatePhase"

updatePhase :: WizardRequestContextC s m => U.UUID -> DocumentTemplatePhase -> m DocumentTemplateDetailDTO
updatePhase dtUuid phase = do
  dt <- findDocumentTemplateByUuid dtUuid
  when (dt.phase == DraftDocumentTemplatePhase) (throwError . UserError $ _ERROR_SERVICE_MCP__DRAFT_NOT_RELEASED (U.toString dtUuid))
  modifyDocumentTemplate dtUuid (DocumentTemplateChangeDTO phase)

deleteDocumentTemplateTool :: WizardRequestContextC s m => McpTool m
deleteDocumentTemplateTool =
  McpTool
    { name = "delete_document_template"
    , title = "Delete document template"
    , description =
        "Delete a document template version, or with all_versions every version of its id, together with the documents generated from it; "
          ++ "projects using it lose their document template (its dependents in get_document_template). It cannot be undone. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments deleteTemplateArguments)
    , outputSchema = Just $ confirmationOutputSchema (objectSchema [] [])
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "delete_document_template" deleteTemplateArguments prepareTemplateDeletion
    }
  where
    deleteTemplateArguments = (,) <$> templateUuidArgument <*> allVersionsArgument

prepareTemplateDeletion :: WizardRequestContextC s m => (U.UUID, Bool) -> m (McpConfirmation m)
prepareTemplateDeletion (dtUuid, allVersions) = do
  dt <- findDocumentTemplateByUuid dtUuid
  dependents <- getDocumentTemplateDependents dtUuid (Just allVersions)
  checkDeleteAllowed dependents
  return
    McpConfirmation
      { summary = "Deletes " ++ deletedVersions allVersions "document template" dt.id (show (createCoordinate dt)) ++ templateDependentsSummary dependents ++ "; it cannot be undone"
      , revision = show dependents
      , perform = object [] <$ deleteDocumentTemplate dtUuid (Just allVersions)
      }

templateDependentsSummary :: LibraryDependents -> String
templateDependentsSummary dependents =
  countsSummary " together with " [(length dependents.documents, "documents")]
    ++ countsSummary "; projects that lose it as their document template: " [(length dependents.projects, "projects")]

exportDocumentTemplate :: WizardRequestContextC s m => McpTool m
exportDocumentTemplate =
  McpTool
    { name = "export_document_template"
    , title = "Export document template"
    , description = "Export a released document template as a zip bundle and get a temporary download url; import it elsewhere with import_document_template"
    , inputSchema = argumentsSchema templateUuidArgument
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpTemporaryFileDTO)
    , annotations = readOnlyAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = Transactional
    , run = withArguments templateUuidArgument (\uuid -> toJSON <$> mcpTemporaryFile (`getTemporaryFileWithBundle` uuid))
    }

importDocumentTemplate :: forall s m. WizardRequestContextC s m => McpTool m
importDocumentTemplate =
  McpTool
    { name = "import_document_template"
    , title = "Import document template"
    , description = "Import a document template zip bundle (as export_document_template or the registry produce it) into a workspace or the organization with tenant: true"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy DocumentTemplateSimple)
    , annotations = writeAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(inScope, content) -> inScope (toJSON <$> importAndConvertBundle (BSL.fromStrict content) False)
    }
  where
    arguments =
      (,)
        <$> scopeArguments
        <*> requiredBase64ContentArgument "content" "The zip bundle, base64 encoded, at most 10 MB"
        :: Arguments (m Value -> m Value, BS.ByteString)

templateUuidArgument :: Arguments U.UUID
templateUuidArgument = requiredArgument "document_template_uuid" "The document template"
