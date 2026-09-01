module Specs.Service.Document.DocumentServiceSpec where

import Data.Foldable (traverse_)
import Test.Hspec hiding (shouldBe)

import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as USR
import Shared.Model.Document.DocumentContext
import Shared.Model.Report.Report
import Shared.Service.Document.Context.DocumentContextService

import Specs.Common
import Specs.Service.Document.Common

documentIntegrationSpec requestContext =
  describe "Document Service Integration" $
    describe "createDocumentContext" $
      it "Successfully created" $
        -- GIVEN: Prepare expectation
        do
          let expectation = dmp1
          -- AND: Run migrations
          runInContextIO USR.runMigration requestContext
          runInContextIO TML.runMigration requestContext
          runInContextIO PRJ.runMigration requestContext
          runInContextIO (insertPackage germanyKmPackage) requestContext
          runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
          -- WHEN:
          (Right result) <- runInContext (createDocumentContext doc1 germanyKmPackage [] project1 Nothing) requestContext
          -- THEN:
          compareDocumentContexts result expectation
