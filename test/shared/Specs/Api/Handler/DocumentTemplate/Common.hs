module Specs.Api.Handler.DocumentTemplate.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Project.Project

import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runDependentsMigrations requestContext = do
  runInContextIO U.runMigration requestContext
  runInContextIO DT_Migration.runMigration requestContext
  runInContextIO KnowledgeModelPackage.runMigration requestContext
  runInContextIO (insertProject project4) requestContext
  runInContextIO (insertDocument project4Document) requestContext

project4Document = doc1 {projectUuid = Just project4.uuid, projectEventUuid = Nothing, createdBy = Nothing} :: Document

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfDocumentTemplateInDB requestContext dt = do
  eTemplate <- runInContextIO (findDocumentTemplateByCoordinate (createCoordinate dt) dt.workspaceUuid) requestContext
  liftIO $ isRight eTemplate `shouldBe` True
  let (Right templateFromDB) = eTemplate
  compareTemplateDtos templateFromDB dt

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareTemplateDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.description `shouldBe` expDto.description
