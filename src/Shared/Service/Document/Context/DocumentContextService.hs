module Shared.Service.Document.Context.DocumentContextService (
  getProjectDocumentContext,
  getKnowledgeModelEditorDocumentContext,
  createProjectDocumentContext,
  createKnowledgeModelEditorDocumentContext,
  withDocument,
) where

import Control.Monad (forM_, unless)
import Control.Monad.Except (throwError)
import Data.Either (partitionEithers)
import qualified Data.List as L
import qualified Data.UUID as U

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectFileDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Localization.Messages.Public
import Shared.Model.Common.Lens
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Event.ProjectEventList
import Shared.Model.Project.Event.ProjectEventListLenses ()
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectContent
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Model.Settings.Settings
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Model.Workspace.Workspace
import Shared.Service.Document.Context.DocumentContextHash
import Shared.Service.Document.Context.DocumentContextMapper
import Shared.Service.KnowledgeModel.Editor.EditorAcl
import qualified Shared.Service.KnowledgeModel.Editor.EditorMapper as EditorMapper
import Shared.Service.KnowledgeModel.KnowledgeModelService
import Shared.Service.Project.Compiler.ProjectCompilerService
import Shared.Service.Project.ProjectAcl
import Shared.Service.Report.ReportGenerator
import Shared.Service.Settings.WorkspaceSettingsService
import Shared.Service.Tenant.TenantHelper
import Shared.Util.List

getProjectDocumentContext :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m DocumentContext
getProjectDocumentContext projectUuid mEventUuid = do
  project <- findProjectByUuid projectUuid
  checkViewPermissionToProject project.workspaceUuid project.visibility project.sharing project.permissions
  fst <$> createProjectDocumentContext project mEventUuid

getKnowledgeModelEditorDocumentContext :: WizardRequestContextC s m => U.UUID -> m DocumentContext
getKnowledgeModelEditorDocumentContext editorUuid = do
  checkPermissionToEditor editorUuid
  editor <- findKnowledgeModelEditorByUuid editorUuid
  createKnowledgeModelEditorDocumentContext editor

createProjectDocumentContext :: WizardRequestContextC s m => Project -> Maybe U.UUID -> m (DocumentContext, Maybe U.UUID)
createProjectDocumentContext project mEventUuid = do
  projectEvents <- findProjectEventListsByProjectUuid project.uuid
  forM_ mEventUuid (validateProjectEventUuid projectEvents)
  let events = maybe projectEvents (\eventUuid -> takeWhileInclusive (\e -> getUuid e /= eventUuid) projectEvents) mEventUuid
  let mCutOff = lastSafe events
  let content = compileProjectEvents events
  km <- compileKnowledgeModelWithCaching' [] (Just project.knowledgeModelPackageUuid) project.selectedQuestionTagUuids True
  pkg <- findPackageByUuid project.knowledgeModelPackageUuid
  workspace <- findWorkspaceByUuid project.workspaceUuid
  mCreatedBy <- maybe (return Nothing) findUserByUuid' project.creatorUuid
  (users, groups) <- heSettingsToPerms project
  mVersion <- maybe (return Nothing) (findProjectVersionByEventUuid' project.uuid . getUuid) mCutOff
  versions <- findVersionsUpTo project events
  files <- findProjectFilesSimpleByProject project.uuid
  report <- generateReport content.phaseUuid km content.replies
  config <- createConfig workspace
  let ctx = toProjectDocumentContext workspace project mCreatedBy users groups content (fmap (.uuid) mVersion) versions files report km pkg config
  return (ctx {hash = computeContextHash ctx}, fmap getUuid mCutOff)

createKnowledgeModelEditorDocumentContext :: WizardRequestContextC s m => KnowledgeModelEditor -> m DocumentContext
createKnowledgeModelEditorDocumentContext editor = do
  kmEvents <- fmap EditorMapper.toKnowledgeModelEvent <$> findKnowledgeModelEventsByEditorUuid editor.uuid
  replies <- EditorMapper.toReplies <$> findKnowledgeModelRepliesByEditorUuid editor.uuid
  km <- compileKnowledgeModelWithCaching' kmEvents editor.previousPackageUuid [] False
  mPreviousPkg <- maybe (return Nothing) findPackageByUuid' editor.previousPackageUuid
  workspace <- findWorkspaceByUuid editor.workspaceUuid
  mCreatedBy <- maybe (return Nothing) findUserByUuid' editor.createdBy
  report <- generateReport Nothing km replies
  config <- createConfig workspace
  let ctx = toKnowledgeModelEditorDocumentContext workspace editor mPreviousPkg mCreatedBy replies report km config
  return ctx {hash = computeContextHash ctx}

withDocument :: Document -> DocumentTemplate -> DocumentTemplateFormatSimple -> Maybe User -> Maybe DocumentTemplateLocale -> DocumentContext -> DocumentContext
withDocument doc dt format mCreatedBy mLocale ctx = ctx {document = Just $ toDocumentContextDocument doc dt format mCreatedBy mLocale}

-- --------------------------------
-- PRIVATE
-- --------------------------------
validateProjectEventUuid :: WizardRequestContextC s m => [ProjectEventList] -> U.UUID -> m ()
validateProjectEventUuid events eventUuid =
  unless (any ((== eventUuid) . getUuid) events) $
    throwError . NotExistsError $
      _ERROR_DATABASE__ENTITY_NOT_FOUND "project_event" [("uuid", U.toString eventUuid)]

findVersionsUpTo :: WizardRequestContextC s m => Project -> [ProjectEventList] -> m [ProjectVersionList]
findVersionsUpTo project events = do
  versions <- findProjectVersionListByProjectUuid project.uuid
  let eventUuids = fmap getUuid events
  return $ filter (\v -> v.eventUuid `elem` eventUuids) versions

createConfig :: WizardRequestContextC s m => Workspace -> m DocumentContextConfig
createConfig workspace = do
  lookAndFeel <- getEffectiveLookAndFeel workspace
  clientUrl <- getClientUrl
  return
    DocumentContextConfig
      { clientUrl = clientUrl
      , appTitle = lookAndFeel.appTitle
      , primaryColor = lookAndFeel.primaryColor
      , logoUrl = lookAndFeel.logoUrl
      }

heSettingsToPerms :: WizardRequestContextC s m => Project -> m ([DocumentContextUserPerm], [DocumentContextUserGroupPerm])
heSettingsToPerms project = do
  perms <- traverse heToDocumentContextPerm project.permissions
  return $ partitionEithers perms

heToDocumentContextPerm :: WizardRequestContextC s m => ProjectPerm -> m (Either DocumentContextUserPerm DocumentContextUserGroupPerm)
heToDocumentContextPerm perm =
  case perm.memberType of
    UserProjectPermType -> do
      user <- findUserByUuid perm.memberUuid
      return . Left $
        DocumentContextUserPerm
          { user = toDocumentContextUser user
          , perms = L.sort perm.perms
          }
    UserGroupProjectPermType -> do
      userGroup <- findUserGroupByUuid perm.memberUuid
      members <- findUsersByUserGroupUuid userGroup.uuid
      return . Right $
        DocumentContextUserGroupPerm
          { group = toDocumentContextUserGroup userGroup members
          , perms = L.sort perm.perms
          }
