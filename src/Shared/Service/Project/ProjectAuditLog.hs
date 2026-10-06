module Shared.Service.Project.ProjectAuditLog where

import Control.Monad (unless)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.AuditLogRef
import Shared.Model.AuditLog.Data.ProjectAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Project
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Service.AuditLog.AuditLogMapper (toUserRef)
import Shared.Service.AuditLog.AuditLogService
import Shared.Service.DocumentTemplate.DocumentTemplateAuditLog (toDocumentTemplateRef)
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageAuditLog (toKnowledgeModelRef)

al_createProject :: WizardRequestContextC s m => Project -> KnowledgeModelPackage -> Maybe Project -> m ()
al_createProject project pkg mProjectTemplate =
  logAuditLog
    projectsCreate
    (projectEntity project)
    AuditLogProjectCreateData
      { knowledgeModel = toKnowledgeModelRef pkg
      , projectTemplate = fmap toProjectRef mProjectTemplate
      , visibility = project.visibility
      , sharing = project.sharing
      }

al_cloneProject :: WizardRequestContextC s m => Project -> Project -> m ()
al_cloneProject originProject project = logAuditLog projectsClone (projectEntity project) AuditLogProjectCloneData {sourceProject = toProjectRef originProject}

al_importProject :: WizardRequestContextC s m => Project -> [User] -> m ()
al_importProject project owners = whenAuditLogEnabled $ do
  pkg <- findPackageByUuid project.knowledgeModelPackageUuid
  logAuditLog projectsImport (projectEntity project) AuditLogProjectImportData {knowledgeModel = toKnowledgeModelRef pkg, owners = fmap toUserRef owners}

al_updateProject :: WizardRequestContextC s m => Project -> Project -> m ()
al_updateProject project updatedProject = whenAuditLogEnabled $ do
  documentTemplateChange <- refChange (traverse findDocumentTemplateRef) project.documentTemplateUuid updatedProject.documentTemplateUuid
  formatChange <- refChange findFormatRef (formatKey project) (formatKey updatedProject)
  logAuditLogChanges
    projectsUpdate
    (projectEntity updatedProject)
    AuditLogProjectChanges
      { name = change project.name updatedProject.name
      , description = change project.description updatedProject.description
      , visibility = change project.visibility updatedProject.visibility
      , sharing = change project.sharing updatedProject.sharing
      , projectTags = change project.projectTags updatedProject.projectTags
      , isTemplate = change project.isTemplate updatedProject.isTemplate
      , documentTemplate = documentTemplateChange
      , format = formatChange
      }
  where
    formatKey p = (p.documentTemplateUuid, p.formatUuid)

al_updateProjectPermissions :: WizardRequestContextC s m => Project -> Project -> m ()
al_updateProjectPermissions project updatedProject = do
  let oldPerms = fmap sortPerms project.permissions
  let newPerms = fmap sortPerms updatedProject.permissions
  unless (oldPerms == newPerms) . whenAuditLogEnabled $ do
    added <- traverse toProjectMember (newPerms L.\\ oldPerms)
    removed <- traverse toProjectMember (oldPerms L.\\ newPerms)
    logAuditLog projectsPermissionsUpdate (projectEntity updatedProject) AuditLogProjectPermissionsData {added = added, removed = removed}
  where
    sortPerms perm = perm {perms = L.sort perm.perms} :: ProjectPerm

al_createProjectVersion :: WizardRequestContextC s m => Project -> ProjectVersion -> m ()
al_createProjectVersion project version = logAuditLog projectsVersionCreate (projectEntity project) (toVersionData version)

al_updateProjectVersion :: WizardRequestContextC s m => Project -> ProjectVersion -> ProjectVersion -> m ()
al_updateProjectVersion project version updatedVersion =
  logAuditLogUpdate
    projectsVersionUpdate
    (projectEntity project)
    AuditLogProjectVersionChanges {name = change version.name updatedVersion.name, description = change version.description updatedVersion.description}
    (toVersionData updatedVersion)

al_deleteProjectVersion :: WizardRequestContextC s m => Project -> ProjectVersion -> m ()
al_deleteProjectVersion project version = logAuditLog projectsVersionDelete (projectEntity project) (toVersionData version)

al_revertProject :: WizardRequestContextC s m => Project -> U.UUID -> UTCTime -> m ()
al_revertProject project eventUuid eventCreatedAt =
  logAuditLog projectsRevert (projectEntity project) AuditLogProjectRevertData {eventUuid = eventUuid, eventCreatedAt = eventCreatedAt}

al_migrateProject :: WizardRequestContextC s m => Project -> KnowledgeModelPackage -> m ()
al_migrateProject project targetPkg = whenAuditLogEnabled $ do
  sourcePkg <- findPackageByUuid project.knowledgeModelPackageUuid
  logAuditLog
    projectsMigrate
    (projectEntity project)
    AuditLogProjectMigrateData {sourceKnowledgeModel = toKnowledgeModelRef sourcePkg, targetKnowledgeModel = toKnowledgeModelRef targetPkg}

al_deleteProject :: WizardRequestContextC s m => Project -> m ()
al_deleteProject project = logAuditLogNoData projectsDelete (projectEntity project)

projectEntity :: Project -> AuditLogEntity
projectEntity project = entityInWorkspace project.workspaceUuid project.uuid project.name

toProjectRef :: Project -> AuditLogRef
toProjectRef project = AuditLogRef {uuid = project.uuid, name = project.name}

toVersionData :: ProjectVersion -> AuditLogProjectVersionData
toVersionData version = AuditLogProjectVersionData {version = AuditLogRef {uuid = version.uuid, name = version.name}}

toProjectMember :: WizardRequestContextC s m => ProjectPerm -> m AuditLogProjectMember
toProjectMember perm = do
  memberName <- findMemberName perm.memberType perm.memberUuid
  return AuditLogProjectMember {memberType = perm.memberType, member = AuditLogRef {uuid = perm.memberUuid, name = memberName}, perms = perm.perms}

findMemberName :: WizardRequestContextC s m => ProjectPermType -> U.UUID -> m String
findMemberName UserProjectPermType userUuid = (.email) <$> findUserByUuid userUuid
findMemberName UserGroupProjectPermType userGroupUuid = (.name) <$> findUserGroupByUuid userGroupUuid

findDocumentTemplateRef :: WizardRequestContextC s m => U.UUID -> m AuditLogDocumentTemplateRef
findDocumentTemplateRef documentTemplateUuid = toDocumentTemplateRef <$> findDocumentTemplateByUuid documentTemplateUuid

findFormatRef :: WizardRequestContextC s m => (Maybe U.UUID, Maybe U.UUID) -> m (Maybe AuditLogRef)
findFormatRef (Just documentTemplateUuid, Just formatUuid) = Just . toFormatRef <$> findDocumentTemplateFormatByDocumentTemplateIdAndUuid documentTemplateUuid formatUuid
findFormatRef _ = return Nothing

toFormatRef :: DocumentTemplateFormatSimple -> AuditLogRef
toFormatRef format = AuditLogRef {uuid = format.uuid, name = format.name}

refChange :: (Monad m, Eq k, Eq r) => (k -> m r) -> k -> k -> m (Maybe (Change r))
refChange findRef oldKey newKey
  | oldKey == newKey = return Nothing
  | otherwise = change <$> findRef oldKey <*> findRef newKey
