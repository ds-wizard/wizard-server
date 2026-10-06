module Shared.Service.Project.ProjectUtil where

import Control.Monad (when)
import qualified Data.List as L
import Data.Maybe (isJust)
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
import Shared.Model.Settings.Settings
import Shared.Service.Project.ProjectMapper
import Shared.Service.Settings.WorkspaceSettingsService
import Shared.Util.Reference

extractVisibility workspaceUuid dto = do
  tcProject <- getEffectiveSettingsProjects (Just workspaceUuid)
  if tcProject.projectVisibility.enabled
    then return dto.visibility
    else return tcProject.projectVisibility.defaultValue

extractSharing workspaceUuid dto = do
  tcProject <- getEffectiveSettingsProjects (Just workspaceUuid)
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

getKnowledgeModelProjectState :: WizardRequestContextC s m => KnowledgeModelPackage -> Maybe U.UUID -> m KnowledgeModelProjectState
getKnowledgeModelProjectState pkg mWorkspaceUuid = do
  mLatestPkg <- findLatestPackageById' pkg.id (Just ReleasedKnowledgeModelPackagePhase) mWorkspaceUuid
  case mLatestPkg of
    Just latestPkg ->
      if latestPkg.uuid == pkg.uuid
        then return UpToDateKnowledgeModelProjectState
        else return OutdatedKnowledgeModelProjectState
    Nothing -> return UpToDateKnowledgeModelProjectState

getDocumentTemplateProjectState :: WizardRequestContextC s m => Maybe U.UUID -> Maybe U.UUID -> m (Maybe DocumentTemplateProjectState)
getDocumentTemplateProjectState mDocumentTemplateUuid mWorkspaceUuid =
  case mDocumentTemplateUuid of
    Nothing -> return Nothing
    Just documentTemplateUuid -> do
      documentTemplate <- findDocumentTemplateByUuid documentTemplateUuid
      templates <- findDocumentTemplatesById documentTemplate.id mWorkspaceUuid
      let releasedTemplates = filter (\t -> t.phase == ReleasedDocumentTemplatePhase) templates
      case releasedTemplates of
        [] -> return (Just UpToDateDocumentTemplateProjectState)
        _ ->
          let latestTemplate = L.maximumBy (\t1 t2 -> compareVersion t1.version t2.version <> compare (isJust t1.workspaceUuid) (isJust t2.workspaceUuid)) releasedTemplates
           in if latestTemplate.uuid == documentTemplate.uuid
                then return (Just UpToDateDocumentTemplateProjectState)
                else return (Just OutdatedDocumentTemplateProjectState)

skipIfAssigningProject :: WizardRequestContextC s m => Project -> m () -> m ()
skipIfAssigningProject project action = do
  tcProject <- getEffectiveSettingsProjects (Just project.workspaceUuid)
  let projectSharingEnabled = tcProject.projectSharing.enabled
  let projectSharingAnonymousEnabled = tcProject.projectSharing.anonymousEnabled
  when
    (not (projectSharingEnabled && projectSharingAnonymousEnabled) || (not . null $ project.permissions))
    action
