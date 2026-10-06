module Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftData
import Shared.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Shared.Model.DocumentTemplate.DocumentTemplateDraftList
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Model.Error.Error
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetService
import Shared.Service.DocumentTemplate.DocumentTemplateAcl
import Shared.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Service.DocumentTemplate.DocumentTemplateValidation hiding (validateChangeDto)
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftValidation
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileService
import Shared.Service.DocumentTemplate.Locale.Pot.PotFileService
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.Uuid

getDraftsPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page DocumentTemplateDraftList)
getDraftsPage mQuery pageable sort = do
  checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
  findDraftsPage mQuery pageable sort

createDraft :: WizardRequestContextC s m => DocumentTemplateDraftCreateDTO -> m DocumentTemplateSimple
createDraft reqDto =
  runInTransaction $ do
    workspaceUuid <- requireTenantOrWorkspaceScope
    checkPermissionInWorkspace _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION workspaceUuid
    checkDocumentTemplateDraftLimit
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks (.tenantUuid')
    case reqDto.basedOn of
      Just dtUuid -> do
        tml <- findDocumentTemplateByUuid dtUuid
        checkTemplateInWorkspace workspaceUuid tml.workspaceUuid
        formats <- findDocumentTemplateFormats dtUuid
        when
          tml.nonEditable
          (throwError . UserError $ _ERROR_SERVICE_DOC_TML__NON_EDITABLE_DOC_TML)
        let (draft, draftFormats) = fromCreateDTO reqDto uuid tml formats workspaceUuid now
        validateNewDocumentTemplate draft False
        insertDocumentTemplate draft
        traverse_ insertDocumentTemplateFormat draftFormats
        assets <- findAssetsByDocumentTemplateUuid dtUuid
        traverse_ (duplicateAsset draft.uuid) assets
        files <- findFilesByDocumentTemplateUuid dtUuid
        traverse_ (duplicateFile draft.uuid) files
        let draftData = fromCreateDraftData draft
        insertDraftData draftData
        return $ toSimple draft
      Nothing -> do
        let draft = fromCreateDTO' reqDto uuid tenantUuid workspaceUuid now
        validateNewDocumentTemplate draft False
        insertDocumentTemplate draft
        let draftData = fromCreateDraftData draft
        insertDraftData draftData
        return $ toSimple draft

getDraft :: WizardRequestContextC s m => U.UUID -> m DocumentTemplateDraftDetail
getDraft dtUuid = do
  checkEditorPermissionToDocumentTemplate dtUuid
  draft <- findDraftByUuid dtUuid
  formats <- findDocumentTemplateFormats draft.uuid
  draftData <- findDraftDataByUuid dtUuid
  mProjectSuggestion <-
    case draftData.projectUuid of
      Just projectUuid -> findProjectSuggestionByUuid' projectUuid
      Nothing -> return Nothing
  mKmEditorSuggestion <-
    case draftData.knowledgeModelEditorUuid of
      Just knowledgeModelEditorUuid -> findKnowledgeModelEditorSuggestionByUuid' knowledgeModelEditorUuid
      Nothing -> return Nothing
  return $ toDraftDetail draft formats draftData mProjectSuggestion mKmEditorSuggestion

modifyDraft :: WizardRequestContextC s m => U.UUID -> DocumentTemplateDraftChangeDTO -> m DocumentTemplateDraftDetail
modifyDraft dtUuid reqDto =
  runInTransaction $ do
    checkEditorPermissionToDocumentTemplate dtUuid
    -- Update draft
    now <- liftIO getCurrentTime
    draft <- findDraftByUuid dtUuid
    validateChangeDto reqDto draft
    let draftUpdated = fromChangeDTO reqDto draft now
    updateDocumentTemplateById draftUpdated
    -- Update formats
    let formatsUpdated = fmap (fromFormatDTO dtUuid draft.tenantUuid draft.createdAt now) reqDto.formats
    traverse_ insertOrUpdateDocumentTemplateFormat formatsUpdated
    deleteDocumentTemplateFormatsExcept dtUuid (fmap (.uuid) formatsUpdated)
    -- Delete temporary documents for the template
    deleteTemporalDocumentsByDocumentTemplateUuid dtUuid
    when
      (reqDto.phase == ReleasedDocumentTemplatePhase)
      ( do
          void $ deleteDraftDataByDocumentTemplateUuid dtUuid
          publishGeneratePotFileCommand draftUpdated
      )
    return $ toDraftDetail' draftUpdated formatsUpdated

modifyDraftData :: WizardRequestContextC s m => U.UUID -> DocumentTemplateDraftDataChangeDTO -> m DocumentTemplateDraftDataDTO
modifyDraftData dtUuid reqDto =
  runInTransaction $ do
    checkEditorPermissionToDocumentTemplate dtUuid
    draftData <- findDraftDataByUuid dtUuid
    let updatedDraftData = fromDraftDataChangeDTO draftData reqDto
    updateDraftDataById updatedDraftData
    mProjectSuggestion <-
      case updatedDraftData.projectUuid of
        Just projectUuid -> findProjectSuggestionByUuid' projectUuid
        Nothing -> return Nothing
    mKmEditorSuggestion <-
      case draftData.knowledgeModelEditorUuid of
        Just knowledgeModelEditorUuid -> findKnowledgeModelEditorSuggestionByUuid' knowledgeModelEditorUuid
        Nothing -> return Nothing
    return $ toDraftDataDTO updatedDraftData mProjectSuggestion mKmEditorSuggestion

deleteDraft :: WizardRequestContextC s m => U.UUID -> m ()
deleteDraft uuid =
  runInTransaction $ do
    checkEditorPermissionToDocumentTemplate uuid
    draft <- findDraftByUuid uuid
    void $ deleteDraftByUuid uuid
