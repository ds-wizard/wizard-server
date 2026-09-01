module Specs.Api.Handler.Project.Common where

import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.Tenant.Tenant
import Shared.Service.Tenant.Config.ConfigService

import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfProjectInDB requestContext project projectEvents = do
  eProject <- runInContextIO (findProjectByUuid project.uuid) requestContext
  liftIO $ isRight eProject `shouldBe` True
  let (Right projectFromDb) = eProject
  compareProjectDtos projectFromDb project
  eProjectEvents <- runInContextIO (findProjectEventsByProjectUuid project.uuid) requestContext
  liftIO $ isRight eProjectEvents `shouldBe` True
  let (Right projectEventsFromDb) = eProjectEvents
  liftIO $ projectEventsFromDb `shouldBe` projectEvents

assertExistenceOfProjectContentInDB requestContext projectUuid content = do
  eProjectEvents <- runInContextIO (findProjectEventsByProjectUuid projectUuid) requestContext
  liftIO $ isRight eProjectEvents `shouldBe` True
  let (Right projectEventsFromDb) = eProjectEvents
  compareProjectContentDtos projectEventsFromDb content

assertAbsenceOfProjectInDB requestContext project = do
  eProject <- runInContextIO (findProjectByUuid project.uuid) requestContext
  liftIO $ isLeft eProject `shouldBe` True
  let (Left error) = eProject
  liftIO $
    error
      `shouldBe` NotExistsError
        ( _ERROR_DATABASE__ENTITY_NOT_FOUND
            "project"
            [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString project.uuid)]
        )

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareProjectCreateDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.knowledgeModelPackage `shouldBe` expDto.knowledgeModelPackage

compareProjectCreateFromTemplateDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldNotBe` expDto.uuid
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.knowledgeModelState `shouldBe` expDto.knowledgeModelState
  liftIO $ resDto.documentTemplateState `shouldBe` expDto.documentTemplateState
  liftIO $ resDto.knowledgeModelPackage `shouldBe` expDto.knowledgeModelPackage

compareProjectCloneDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldNotBe` expDto.uuid
  liftIO $ resDto.name `shouldBe` ("Copy of " ++ expDto.name)
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.knowledgeModelState `shouldBe` expDto.knowledgeModelState
  liftIO $ resDto.documentTemplateState `shouldBe` expDto.documentTemplateState
  liftIO $ resDto.knowledgeModelPackage `shouldBe` expDto.knowledgeModelPackage

compareProjectCreateDtos' resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.phaseUuid `shouldBe` expDto.phaseUuid
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.knowledgeModelState `shouldBe` expDto.knowledgeModelState
  liftIO $ resDto.documentTemplateState `shouldBe` expDto.documentTemplateState
  liftIO $ resDto.knowledgeModelPackage `shouldBe` expDto.knowledgeModelPackage
  liftIO $ resDto.selectedQuestionTagUuids `shouldBe` expDto.selectedQuestionTagUuids
  liftIO $ resDto.knowledgeModel `shouldBe` expDto.knowledgeModel
  liftIO $ resDto.replies `shouldBe` expDto.replies

compareProjectCreateDtos'' resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.phaseUuid `shouldBe` expDto.phaseUuid
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.selectedQuestionTagUuids `shouldBe` expDto.selectedQuestionTagUuids
  liftIO $ resDto.knowledgeModel `shouldBe` expDto.knowledgeModel
  liftIO $ resDto.replies `shouldBe` expDto.replies

compareProjectDtos resDto expDto = liftIO $ resDto `shouldBe` expDto

compareProjectContentDtos resDto expDto =
  liftIO $ resDto `shouldBe` expDto

compareReportDtos resDto expDto = do
  liftIO $ resDto.totalReport `shouldBe` expDto.totalReport
  liftIO $ resDto.chapterReports `shouldBe` expDto.chapterReports

-- --------------------------------
-- HELPERS
-- --------------------------------
updateAnonymousProjectSharing requestContext value = do
  (Right tcProject) <- runInContextIO getCurrentTenantConfigProject requestContext
  let tcProjectUpdated = tcProject {projectSharing = tcProject.projectSharing {anonymousEnabled = value}}
  runInContextIO (modifyTenantConfigProject tcProjectUpdated) requestContext
