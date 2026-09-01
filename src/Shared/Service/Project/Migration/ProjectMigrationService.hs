module Shared.Service.Project.Migration.ProjectMigrationService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO (findPackageByUuid)
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Event.ProjectEventList
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectContent
import Shared.Service.KnowledgeModel.KnowledgeModelService
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.Project.Compiler.ProjectCompilerService
import Shared.Service.Project.Event.ProjectEventMapper
import Shared.Service.Project.Migration.Migrator.Sanitizer
import Shared.Service.Project.Migration.ProjectMigrationAudit
import Shared.Service.Project.Migration.ProjectMigrationMapper
import Shared.Service.Project.ProjectAcl
import Shared.Service.Project.ProjectService
import Shared.Util.List
import Shared.Util.Uuid

migrateProject :: WizardRequestContextC s m => U.UUID -> ProjectMigrationCreateDTO -> m ProjectDetailQuestionnaireDTO
migrateProject projectUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkMigrationPermissionToProject project.visibility project.permissions
    newPkg <- findPackageByUuid reqDto.targetKnowledgeModelPackageUuid
    oldKm <- compileKnowledgeModel [] (Just project.knowledgeModelPackageUuid) reqDto.targetTagUuids
    newKm <- compileKnowledgeModel [] (Just reqDto.targetKnowledgeModelPackageUuid) reqDto.targetTagUuids
    projectEvents <- findProjectEventListsByProjectUuid project.uuid
    deltaEvents <- sanitizeProjectEvents oldKm newKm projectEvents
    phaseEvents <- ensurePhaseIsSetIfNecessary project newKm projectEvents
    (newDocumentTemplateUuid, newFormatUuid) <- getNewDocumentTemplateIdAndFormatUuid project newPkg
    now <- liftIO getCurrentTime
    let updatedProject =
          project
            { knowledgeModelPackageUuid = reqDto.targetKnowledgeModelPackageUuid
            , selectedQuestionTagUuids = reqDto.targetTagUuids
            , documentTemplateUuid = newDocumentTemplateUuid
            , formatUuid = newFormatUuid
            , squashed = False
            , updatedAt = now
            }
            :: Project
    updateProjectByUuid updatedProject
    insertProjectEvents (fmap (toEvent project.uuid project.tenantUuid) deltaEvents ++ phaseEvents)
    auditProjectMigration reqDto project
    logOutOnlineUsersWhenProjectDramaticallyChanged project.uuid
    getProjectDetailQuestionnaireByUuid project.uuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
ensurePhaseIsSetIfNecessary :: WizardRequestContextC s m => Project -> KnowledgeModel -> [ProjectEventList] -> m [ProjectEvent]
ensurePhaseIsSetIfNecessary project newKm projectEvents = do
  uuid <- liftIO generateUuid
  mCurrentUser <- asks (.currentUser')
  now <- liftIO getCurrentTime
  let projectContent = compileProjectEvents projectEvents
  return $
    case (headSafe newKm.phaseUuids, projectContent.phaseUuid) of
      (Nothing, Nothing) -> []
      (Nothing, Just projectPhaseUuid) -> [toProjectPhaseEvent uuid Nothing project.uuid project.tenantUuid mCurrentUser now]
      (Just kmPhaseUuid, Nothing) -> [toProjectPhaseEvent uuid (Just kmPhaseUuid) project.uuid project.tenantUuid mCurrentUser now]
      (Just kmPhaseUuid, Just projectPhaseUuid)
        | projectPhaseUuid `notElem` newKm.phaseUuids -> [toProjectPhaseEvent uuid (Just kmPhaseUuid) project.uuid project.tenantUuid mCurrentUser now]
        | otherwise -> []

getNewDocumentTemplateIdAndFormatUuid :: WizardRequestContextC s m => Project -> KnowledgeModelPackage -> m (Maybe U.UUID, Maybe U.UUID)
getNewDocumentTemplateIdAndFormatUuid oldProject newPkg = do
  case oldProject.documentTemplateUuid of
    Just dtUuid -> do
      documentTemplate <- findDocumentTemplateByUuid dtUuid
      if fitsIntoKMSpecs (createCoordinate newPkg) documentTemplate.allowedPackages
        then return (Just dtUuid, oldProject.formatUuid)
        else return (Nothing, Nothing)
    Nothing -> return (Nothing, Nothing)
