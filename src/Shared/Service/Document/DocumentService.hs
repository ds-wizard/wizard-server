module Shared.Service.Document.DocumentService where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import qualified Data.Map as M
import Shared.Api.Resource.Document.DocumentCreateDTO
import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Constant.Component
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Common.Lens
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftData
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Event.ProjectEventListLenses ()
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectContent
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.S3.Document.DocumentS3
import Shared.Service.Document.Context.DocumentContextService
import Shared.Service.Document.DocumentAcl
import Shared.Service.Document.DocumentMapper
import Shared.Service.Document.DocumentUtil
import Shared.Service.DocumentTemplate.DocumentTemplateService
import Shared.Service.DocumentTemplate.DocumentTemplateValidation
import Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleValidation
import qualified Shared.Service.KnowledgeModel.Editor.EditorMapper as EditorMapper
import Shared.Service.Project.Compiler.ProjectCompilerService
import Shared.Service.Project.ProjectAcl
import qualified Shared.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import Shared.Service.TemporaryFile.TemporaryFileService
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Util.List
import Shared.Util.Logger
import Shared.Util.Uuid

getDocumentsPageDto :: WizardRequestContextC s m => Maybe U.UUID -> Maybe U.UUID -> Maybe String -> Pageable -> [Sort] -> m (Page DocumentDTO)
getDocumentsPageDto mProjectUuid mDocumentTemplateUuid mQuery pageable sort = do
  checkPermission _PROJECTS_EDIT_ROLE_PERMISSION
  docPage <- findDocumentsPage mProjectUuid Nothing mDocumentTemplateUuid mQuery pageable sort
  traverse enhanceDocument docPage

getDocumentsForProject :: WizardRequestContextC s m => U.UUID -> Maybe String -> Pageable -> [Sort] -> m (Page DocumentDTO)
getDocumentsForProject projectUuid mQuery pageable sort = do
  project <- findProjectByUuid projectUuid
  checkViewPermissionToDoc' project
  docPage <- findDocumentsPage (Just projectUuid) (Just project.name) Nothing mQuery pageable sort
  traverse enhanceDocument docPage

createDocument :: WizardRequestContextC s m => DocumentCreateDTO -> m DocumentDTO
createDocument reqDto =
  runInTransaction $ do
    checkEditPermissionToDoc (Just reqDto.projectUuid)
    checkDocumentLimit
    checkStorageSize 0
    project <- findProjectByUuid reqDto.projectUuid
    tml <- getDocumentTemplateByUuidAndPackageId reqDto.documentTemplateUuid project.knowledgeModelPackageUuid
    format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid reqDto.documentTemplateUuid reqDto.formatUuid
    validateMetamodelVersion tml
    validateLanguageAvailability tml reqDto.language
    uuid <- liftIO generateUuid
    mCurrentUser <- asks (.currentUser')
    now <- liftIO getCurrentTime
    projectEvents <- findProjectEventListsByProjectUuid project.uuid
    let filteredProjectEvents =
          case reqDto.projectEventUuid of
            Just eventUuid -> takeWhileInclusive (\e -> getUuid e /= eventUuid) projectEvents
            Nothing -> projectEvents
    let projectContent = compileProjectEvents filteredProjectEvents
    tcOrganization <- findTenantConfigOrganization
    projectVersions <- findProjectVersionsByProjectUuid project.uuid
    let docContextHash = computeHash [] project projectVersions projectContent.phaseUuid projectContent.replies tcOrganization mCurrentUser
    let doc = fromCreateDTO reqDto uuid docContextHash filteredProjectEvents mCurrentUser project.tenantUuid now
    insertDocument doc
    pkg <- findPackageByUuid project.knowledgeModelPackageUuid
    publishToPersistentCommandQueue doc pkg [] project Nothing
    return $ toDTOWithDocTemplate doc project Nothing [] tml format

deleteDocument :: WizardRequestContextC s m => U.UUID -> m ()
deleteDocument docUuid =
  runInTransaction $ do
    doc <- findDocumentByUuid docUuid
    checkEditPermissionToDoc doc.projectUuid
    void $ deleteDocumentByUuid docUuid

downloadDocument :: WizardRequestContextC s m => U.UUID -> m TemporaryFileDTO
downloadDocument docUuid = do
  runInTransaction $ do
    doc <- findDocumentByUuid docUuid
    checkViewPermissionToDoc doc.projectUuid
    content <- retrieveDocumentContent docUuid
    let fileName = fromMaybe "export" doc.fileName
    let contentType = fromMaybe "text/plain" doc.contentType
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile fileName "application/octet-stream" mCurrentUserUuid (BSL.fromStrict content)
    return $ TemporaryFileMapper.toDTO url contentType

createDocumentPreviewForProject :: WizardRequestContextC s m => U.UUID -> m (Document, TemporaryFileDTO)
createDocumentPreviewForProject projectUuid =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkViewPermissionToProject project.visibility project.sharing project.permissions
    case (project.documentTemplateUuid, project.formatUuid) of
      (Just dtUuid, Just formatUuid) -> do
        tml <- getDocumentTemplateByUuidAndPackageId dtUuid project.knowledgeModelPackageUuid
        pkg <- findPackageByUuid project.knowledgeModelPackageUuid
        projectEvents <- findProjectEventListsByProjectUuid projectUuid
        let projectEventUuid = fmap getUuid (lastSafe projectEvents)
        let projectContent = compileProjectEvents projectEvents
        projectVersions <- findProjectVersionsByProjectUuid project.uuid
        createDocumentPreview tml pkg [] project projectVersions projectEventUuid projectContent.phaseUuid projectContent.replies formatUuid project.documentTemplateLanguage False
      _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__TEMPLATE_OR_FORMAT_NOT_SET_UP

createDocumentPreviewForDocTmlDraft :: WizardRequestContextC s m => U.UUID -> m (Document, TemporaryFileDTO)
createDocumentPreviewForDocTmlDraft dtUuid =
  runInTransaction $ do
    draftData <- findDraftDataByUuid dtUuid
    case (draftData.projectUuid, draftData.knowledgeModelEditorUuid, draftData.formatUuid) of
      (Just projectUuid, _, Just formatUuid) -> do
        draft <- findDraftByUuid dtUuid
        project <- findProjectByUuid projectUuid
        pkg <- findPackageByUuid project.knowledgeModelPackageUuid
        checkViewPermissionToProject project.visibility project.sharing project.permissions
        projectEvents <- findProjectEventListsByProjectUuid project.uuid
        let projectEventUuid = fmap getUuid (lastSafe projectEvents)
        let projectContent = compileProjectEvents projectEvents
        projectVersions <- findProjectVersionsByProjectUuid project.uuid
        createDocumentPreview draft pkg [] project projectVersions projectEventUuid projectContent.phaseUuid projectContent.replies formatUuid Nothing False
      (_, Just kmEditorUuid, Just formatUuid) -> do
        draft <- findDraftByUuid dtUuid
        let pkg = toTemporaryPackage draft.tenantUuid draft.createdAt
        editor <- findKnowledgeModelEditorByUuid kmEditorUuid
        kmEditorEvents <- findKnowledgeModelEventsByEditorUuid kmEditorUuid
        let kmEvents = fmap EditorMapper.toKnowledgeModelEvent kmEditorEvents
        kmEditorReplies <- findKnowledgeModelRepliesByEditorUuid kmEditorUuid
        let replies = EditorMapper.toReplies kmEditorReplies
        checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
        mCurrentUser <- asks (.currentUser')
        let project = toTemporaryProject editor pkg mCurrentUser
        let projectEventUuid = Nothing
        createDocumentPreview draft pkg kmEvents project [] projectEventUuid Nothing replies formatUuid Nothing True
      _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__PROJECT_OR_FORMAT_NOT_SET_UP

createDocumentPreview :: WizardRequestContextC s m => DocumentTemplate -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> Project -> [ProjectVersion] -> Maybe U.UUID -> Maybe U.UUID -> M.Map String Reply -> U.UUID -> Maybe String -> Bool -> m (Document, TemporaryFileDTO)
createDocumentPreview dt pkg kmEditorEvents project projectVersions projectEventUuid phaseUuid replies formatUuid mLanguage fromKnowledgeModelEditor = do
  tcOrganization <- findTenantConfigOrganization
  mCurrentUser <- asks (.currentUser')
  let repliesHash = computeHash kmEditorEvents project projectVersions phaseUuid replies tcOrganization mCurrentUser
  logDebugI _CMP_SERVICE ("Replies hash: " ++ show repliesHash)
  docs <-
    if fromKnowledgeModelEditor
      then findDocumentsForCurrentTenantFiltered [("project_replies_hash", show repliesHash), ("durability", "TemporallyDocumentDurability")]
      else findDocumentsForCurrentTenantFiltered [("project_uuid", U.toString project.uuid), ("project_replies_hash", show repliesHash), ("durability", "TemporallyDocumentDurability")]
  case filter (filterAlreadyDoneDocument dt.uuid formatUuid mLanguage) docs of
    (doc : _) -> do
      logInfoI _CMP_SERVICE "Retrieving from cache"
      if doc.state == DoneDocumentState
        then do
          let expirationInSeconds = 60
          link <- presignGetDocumentUrl doc.uuid expirationInSeconds
          return (doc, TemporaryFileDTO link (fromMaybe "text/plain" doc.contentType))
        else return (doc, TemporaryFileMapper.emptyFileDTO)
    [] ->
      case filter (\d -> d.state == QueuedDocumentState || d.state == InProgressDocumentState) docs of
        (doc : _) -> do
          logInfoI _CMP_SERVICE "Waiting to generation"
          return (doc, TemporaryFileMapper.emptyFileDTO)
        _ -> do
          logInfoI _CMP_SERVICE "Generating new preview"
          validateMetamodelVersion dt
          dUuid <- liftIO generateUuid
          now <- liftIO getCurrentTime
          let doc = fromTemporallyCreateDTO dUuid project projectEventUuid dt.uuid formatUuid mLanguage repliesHash mCurrentUser tcOrganization.tenantUuid now fromKnowledgeModelEditor
          insertDocument doc
          let mReplies = if fromKnowledgeModelEditor then Just replies else Nothing
          publishToPersistentCommandQueue doc pkg kmEditorEvents project mReplies
          return (doc, TemporaryFileMapper.emptyFileDTO)

publishToPersistentCommandQueue :: WizardRequestContextC s m => Document -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> Project -> Maybe (M.Map String Reply) -> m ()
publishToPersistentCommandQueue doc pkg kmEditorEvents project mReplies = do
  docContext <- createDocumentContext doc pkg kmEditorEvents project mReplies
  pUuid <- liftIO generateUuid
  let command = toDocPersistentCommand pUuid docContext doc
  insertPersistentCommand command
  return ()
