module Specs.Api.Handler.KnowledgeModelPackage.Detail_DELETE (
  detail_DELETE,
) where

import qualified Data.ByteString.Char8 as BS
import Data.Either
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
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
      assertCountInDB (findPackagesByOrganizationIdAndKmIdInWorkspace "org.nl" "core-nl" Nothing) requestContext 0
      assertCountInDB (findPackagesByOrganizationIdAndKmIdInWorkspace "org.nl" "core-nl" (Just defaultWorkspaceUuid)) requestContext 1

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
      assertCountInDB (findPackagesByOrganizationIdAndKmIdInWorkspace "org.nl" "core-nl" (Just defaultWorkspaceUuid)) requestContext 0
      assertCountInDB (findPackagesByOrganizationIdAndKmIdInWorkspace "org.nl" "core-nl" Nothing) requestContext 2

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
