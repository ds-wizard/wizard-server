module Shared.Service.Project.ProjectUtil where

import Control.Monad (when)
import qualified Data.List as L
import qualified Data.UUID as U

import Shared.Api.Resource.Project.Acl.ProjectPermDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Event.ProjectEventLenses ()
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectState
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Service.Project.ProjectMapper
import Shared.Service.Tenant.Config.ConfigService
import Shared.Util.Coordinate

extractVisibility dto = do
  tcProject <- getCurrentTenantConfigProject
  if tcProject.projectVisibility.enabled
    then return dto.visibility
    else return tcProject.projectVisibility.defaultValue

extractSharing dto = do
  tcProject <- getCurrentTenantConfigProject
  if tcProject.projectSharing.enabled
    then return dto.sharing
    else return tcProject.projectSharing.defaultValue

enhanceProjectPerm :: WizardRequestContextC s m => ProjectPerm -> m ProjectPermDTO
enhanceProjectPerm projectPerm =
  case projectPerm.memberType of
    UserProjectPermType -> do
      user <- findUserByUuid projectPerm.memberUuid
      return $ toUserProjectPermDTO projectPerm user
    UserGroupProjectPermType -> do
      userGroup <- findUserGroupByUuid projectPerm.memberUuid
      return $ toUserGroupProjectPermDTO projectPerm userGroup

getKnowledgeModelProjectState :: WizardRequestContextC s m => KnowledgeModelPackage -> m KnowledgeModelProjectState
getKnowledgeModelProjectState pkg = do
  mLatestPkg <- findLatestPackageByOrganizationIdAndKmId' pkg.organizationId pkg.kmId (Just ReleasedKnowledgeModelPackagePhase)
  case mLatestPkg of
    Just latestPkg ->
      if latestPkg.uuid == pkg.uuid
        then return UpToDateKnowledgeModelProjectState
        else return OutdatedKnowledgeModelProjectState
    Nothing -> return UpToDateKnowledgeModelProjectState

getDocumentTemplateProjectState :: WizardRequestContextC s m => Maybe U.UUID -> m (Maybe DocumentTemplateProjectState)
getDocumentTemplateProjectState mDocumentTemplateUuid =
  case mDocumentTemplateUuid of
    Nothing -> return Nothing
    Just documentTemplateUuid -> do
      documentTemplate <- findDocumentTemplateByUuid documentTemplateUuid
      templates <- findDocumentTemplatesByOrganizationIdAndKmId documentTemplate.organizationId documentTemplate.templateId
      let releasedTemplates = filter (\t -> t.phase == ReleasedDocumentTemplatePhase) templates
      case releasedTemplates of
        [] -> return (Just UpToDateDocumentTemplateProjectState)
        _ ->
          let latestTemplate = L.maximumBy (\t1 t2 -> compareVersion t1.version t2.version) releasedTemplates
           in if latestTemplate.uuid == documentTemplate.uuid
                then return (Just UpToDateDocumentTemplateProjectState)
                else return (Just OutdatedDocumentTemplateProjectState)

skipIfAssigningProject :: WizardRequestContextC s m => Project -> m () -> m ()
skipIfAssigningProject project action = do
  tcProject <- getCurrentTenantConfigProject
  let projectSharingEnabled = tcProject.projectSharing.enabled
  let projectSharingAnonymousEnabled = tcProject.projectSharing.anonymousEnabled
  when
    (not (projectSharingEnabled && projectSharingAnonymousEnabled) || (not . null $ project.permissions))
    action
