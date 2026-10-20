module Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (maximumBy)
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
import Shared.Model.DocumentTemplate.DocumentTemplateDraftPublication
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Model.Error.Error
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetService
import Shared.Service.DocumentTemplate.DocumentTemplateAcl
import Shared.Service.DocumentTemplate.DocumentTemplateAuditLog (al_publishDocumentTemplate)
import Shared.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Service.DocumentTemplate.DocumentTemplateValidation hiding (validateChangeDto)
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftAuditLog
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftValidation
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileService
import Shared.Service.DocumentTemplate.Locale.Pot.PotFileService
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.Date (getCurrentTimeInMicroseconds)
import Shared.Util.Reference (compareVersion)
import Shared.Util.Uuid

getDraftsPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page DocumentTemplateDraftList)
getDraftsPage mQuery pageable sort = do
  checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
  findDraftsPage mQuery pageable sort

createDraft :: WizardRequestContextC s m => DocumentTemplateDraftCreateDTO -> m DocumentTemplateSimple
createDraft reqDto = do
  uuid <- liftIO generateUuid
  createDraftWithUuid uuid reqDto

createDraftWithUuid :: WizardRequestContextC s m => U.UUID -> DocumentTemplateDraftCreateDTO -> m DocumentTemplateSimple
createDraftWithUuid uuid reqDto =
  runInTransaction $ do
    draft <- insertNewDraft uuid reqDto
    al_createDocumentTemplateDraft draft reqDto.basedOn
    return $ toSimple draft

insertNewDraft :: WizardRequestContextC s m => U.UUID -> DocumentTemplateDraftCreateDTO -> m DocumentTemplate
insertNewDraft uuid reqDto =
  runInTransaction $ do
    workspaceUuid <- requireTenantOrWorkspaceScope
    checkPermissionInWorkspace _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION workspaceUuid
    checkDocumentTemplateDraftLimit
    now <- liftIO getCurrentTimeInMicroseconds
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
        return draft
      Nothing -> do
        let draft = fromCreateDTO' reqDto uuid tenantUuid workspaceUuid now
        validateNewDocumentTemplate draft False
        insertDocumentTemplate draft
        let draftData = fromCreateDraftData draft
        insertDraftData draftData
        return draft

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

getDraftToChange :: WizardRequestContextC s m => U.UUID -> m DocumentTemplate
getDraftToChange dtUuid = do
  checkEditorPermissionToAnyDocumentTemplate dtUuid
  dt <- findDocumentTemplateByUuid dtUuid
  validateDraftPhase dt
  return dt

findDraftToChange :: WizardRequestContextC s m => U.UUID -> m (Maybe DocumentTemplate)
findDraftToChange dtUuid = traverse (const (getDraftToChange dtUuid)) =<< findDocumentTemplateByUuid' dtUuid

getDraftChangeDTO :: WizardRequestContextC s m => DocumentTemplate -> m DocumentTemplateDraftChangeDTO
getDraftChangeDTO dt = do
  formats <- findDocumentTemplateFormats dt.uuid
  return ((toChangeDTO dt) {formats = fmap toFormatDTO formats} :: DocumentTemplateDraftChangeDTO)

getDraftPublication :: WizardRequestContextC s m => U.UUID -> m DocumentTemplateDraftPublication
getDraftPublication dtUuid = do
  draft <- getDraftToChange dtUuid
  validateReleasedVersion (toChangeDTO draft) draft
  latestRelease <- findLatestRelease <$> findDocumentTemplatesByIdInWorkspace draft.id draft.workspaceUuid
  formats <- findDocumentTemplateFormats dtUuid
  files <- findFileListsByDocumentTemplateUuid dtUuid
  assets <- findAssetsByDocumentTemplateUuid dtUuid
  return DocumentTemplateDraftPublication {..}

publishDraft :: WizardRequestContextC s m => DocumentTemplate -> m DocumentTemplateDraftDetail
publishDraft draft = do
  reqDto <- getDraftChangeDTO draft
  modifyDraft draft.uuid (reqDto {phase = ReleasedDocumentTemplatePhase} :: DocumentTemplateDraftChangeDTO)

findLatestRelease :: [DocumentTemplate] -> Maybe DocumentTemplate
findLatestRelease templates =
  case filter ((/= DraftDocumentTemplatePhase) . (.phase)) templates of
    [] -> Nothing
    released -> Just (maximumBy (compareVersion `on` (.version)) released)

modifyDraft :: WizardRequestContextC s m => U.UUID -> DocumentTemplateDraftChangeDTO -> m DocumentTemplateDraftDetail
modifyDraft dtUuid reqDto =
  runInTransaction $ do
    checkEditorPermissionToDocumentTemplate dtUuid
    -- Update draft
    now <- liftIO getCurrentTimeInMicroseconds
    draft <- findDraftByUuid dtUuid
    validateChangeDto reqDto draft
    let draftUpdated = fromChangeDTO reqDto draft now
    updateDocumentTemplateById draftUpdated
    al_updateDocumentTemplateDraft draft draftUpdated
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
          al_publishDocumentTemplate draftUpdated
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
    al_deleteDocumentTemplateDraft draft
    void $ deleteDraftByUuid uuid
