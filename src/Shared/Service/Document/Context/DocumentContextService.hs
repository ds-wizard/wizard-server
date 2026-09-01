module Shared.Service.Document.Context.DocumentContextService (
  createDocumentContext,
) where

import Control.Monad (forM)
import Control.Monad.Reader (liftIO)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateLocaleDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectFileDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Model.Common.Lens
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.Document.DocumentContextJM ()
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Event.ProjectEventListLenses ()
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectContent
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.User.UserGroup
import Shared.Service.Document.Context.DocumentContextMapper
import Shared.Service.KnowledgeModel.KnowledgeModelService
import Shared.Service.Project.Compiler.ProjectCompilerService
import Shared.Service.Report.ReportGenerator
import Shared.Service.Tenant.TenantHelper
import qualified Shared.Service.User.Group.UserGroupMapper as UGR_Mapper
import Shared.Util.List

createDocumentContext :: WizardRequestContextC s m => Document -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> Project -> Maybe (M.Map String Reply) -> m DocumentContext
createDocumentContext doc pkg kmEditorEvents project mReplies = do
  km <- compileKnowledgeModelWithCaching' kmEditorEvents (Just project.knowledgeModelPackageUuid) project.selectedQuestionTagUuids (not . null $ kmEditorEvents)
  dt <- findDocumentTemplateByUuid doc.documentTemplateUuid
  mProjectCreatedBy <- forM project.creatorUuid findUserByUuid
  mDocCreatedBy <- forM doc.createdBy findUserByUuid
  tcOrganization <- findTenantConfigOrganization
  tcLookAndFeel <- findTenantConfigLookAndFeel
  clientUrl <- getClientUrl
  now <- liftIO getCurrentTime
  (phaseUuid, replies, labels) <-
    case mReplies of
      Just replies -> return (Nothing, replies, M.empty)
      _ -> do
        projectEvents <- findProjectEventListsByProjectUuid project.uuid
        let filteredProjectEvents =
              case doc.projectEventUuid of
                Just eventUuid -> takeWhileInclusive (\e -> getUuid e /= eventUuid) projectEvents
                Nothing -> projectEvents
        let projectContent = compileProjectEvents filteredProjectEvents
        return (projectContent.phaseUuid, projectContent.replies, projectContent.labels)
  report <- generateReport phaseUuid km replies
  mProjectVersion <-
    case doc.projectEventUuid of
      (Just eventUuid) -> findProjectVersionByEventUuid' project.uuid eventUuid
      _ -> return Nothing
  projectVersionsList <- findProjectVersionListByProjectUuidAndCreatedAt project.uuid (fmap (.createdAt) mProjectVersion)
  projectFiles <-
    case doc.projectUuid of
      Just projectUuid -> findProjectFilesSimpleByProject projectUuid
      Nothing -> return []
  mLocale <-
    case doc.language of
      Just language -> findDocumentTemplateLocaleByDocumentTemplateUuidAndCode' doc.documentTemplateUuid language
      Nothing -> return Nothing
  (users, groups) <- heSettingsToPerms project
  return $
    toDocumentContext
      doc
      clientUrl
      project
      phaseUuid
      replies
      labels
      mProjectVersion
      projectVersionsList
      projectFiles
      km
      dt
      report
      pkg
      tcOrganization
      tcLookAndFeel
      mProjectCreatedBy
      mDocCreatedBy
      mLocale
      users
      groups

-- --------------------------------
-- PRIVATE
-- --------------------------------
findProjectVersionUuid :: U.UUID -> [ProjectVersion] -> Maybe U.UUID
findProjectVersionUuid _ [] = Nothing
findProjectVersionUuid desiredEventUuid (version : rest)
  | desiredEventUuid == version.eventUuid = Just version.uuid
  | otherwise = findProjectVersionUuid desiredEventUuid rest

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
          , perms = perm.perms
          }
    UserGroupProjectPermType -> do
      userGroup <- findUserGroupByUuid perm.memberUuid
      members <- findUsersByUserGroupUuid userGroup.uuid
      return . Right $
        DocumentContextUserGroupPerm
          { group = UGR_Mapper.toDetailDTO userGroup members
          , perms = perm.perms
          }
