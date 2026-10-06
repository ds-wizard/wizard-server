module Specs.Api.Handler.KnowledgeModelPackage.Detail_DELETE (
  detail_DELETE,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Either
import Data.Foldable (traverse_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/knowledge-model-packages/{uuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/knowledge-model-packages/{uuid}" $ do
    test_204 requestContext
    test_204_all_versions_tenant requestContext
    test_204_all_versions_workspace requestContext
    test_204_dependents requestContext
    test_400_hidden_dependents requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = BS.pack $ "/api/knowledge-model-packages/" ++ show netherlandsKmPackageV2.uuid

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Find a result
      eitherPackage <- runInContextIO (getPackageDetailByUuid netherlandsKmPackageV2.uuid True) requestContext
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      liftIO $ isLeft eitherPackage `shouldBe` True
      let (Left (NotExistsError _)) = eitherPackage
      -- AND: We have to end with expression (if there is another way, how to do it, please fix it)
      liftIO $ True `shouldBe` True

test_204_all_versions_tenant requestContext =
  it "HTTP 204 NO CONTENT (allVersions=true keeps the workspace plane)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration requestContext
      runInContextIO (insertPackage netherlandsWorkspaceKmPackageV3) requestContext
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "?allVersions=true") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB (findPackagesByIdInWorkspace "org.nl.core-nl" Nothing) requestContext 0
      assertCountInDB (findPackagesByIdInWorkspace "org.nl.core-nl" (Just defaultWorkspaceUuid)) requestContext 1

test_204_all_versions_workspace requestContext =
  it "HTTP 204 NO CONTENT (allVersions=true keeps the tenant plane)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration requestContext
      runInContextIO (insertPackage netherlandsWorkspaceKmPackageV3) requestContext
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod (BS.pack $ "/api/knowledge-model-packages/" ++ show netherlandsWorkspaceKmPackageV3.uuid ++ "?allVersions=true") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB (findPackagesByIdInWorkspace "org.nl.core-nl" (Just defaultWorkspaceUuid)) requestContext 0
      assertCountInDB (findPackagesByIdInWorkspace "org.nl.core-nl" Nothing) requestContext 2

test_204_dependents requestContext =
  it "HTTP 204 NO CONTENT (derived packages, editors and projects are deleted too)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runDependentsMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod netherlandsKmPackageUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB (findPackagesByIdInWorkspace "org.nl.core-nl" Nothing) requestContext 0
      assertCountInDB findKnowledgeModelEditors requestContext 0
      assertCountInDB findProjects requestContext 0

test_400_hidden_dependents requestContext =
  it "HTTP 400 BAD REQUEST when a dependent is hidden from the caller" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError $ _ERROR_SERVICE_LIBRARY__DELETE_HIDDEN_DEPENDENTS 0 0 1 0 1
      let expBody = encode expDto
      -- AND: Run migrations
      runDependentsMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod netherlandsKmPackageUrl [reqNonAdminAuthHeader, reqCtHeader] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB (findPackagesByIdInWorkspace "org.nl.core-nl" Nothing) requestContext 2
      assertCountInDB findKnowledgeModelEditors requestContext 1
      assertCountInDB findProjects requestContext 1

netherlandsKmPackageUrl = BS.pack $ "/api/knowledge-model-packages/" ++ show netherlandsKmPackage.uuid

runDependentsMigrations requestContext = do
  runInContextIO U.runMigration requestContext
  runInContextIO KnowledgeModelPackage.runMigration requestContext
  runInContextIO (insertDocumentTemplate wizardDocumentTemplate) requestContext
  runInContextIO (traverse_ insertDocumentTemplateFormat wizardDocumentTemplateFormats) requestContext
  runInContextIO (insertProject project4) requestContext
  runInContextIO (insertKnowledgeModelEditor amsterdamKnowledgeModelEditor) requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/knowledge-model-packages/2fc01ee0-4128-4af4-9eb6-c33b52c4d7b4"
    reqHeaders
    reqBody
    "knowledge_model_package"
    [("uuid", "2fc01ee0-4128-4af4-9eb6-c33b52c4d7b4")]
