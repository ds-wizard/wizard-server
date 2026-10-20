module Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.ImportKnowledgeModelPackage (
  import_knowledge_model_package,
) where

import Control.Monad (void)
import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (import_knowledge_model_package)
-- ------------------------------------------------------------------------
import_knowledge_model_package :: RequestContext -> SpecWith ((), Application)
import_knowledge_model_package requestContext =
  describe "POST /api/mcp (import_knowledge_model_package)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["tenant" .= True, "content" .= base64Content (encode netherlandsV2KmBundle)]

reqBody = toolCallBody "import_knowledge_model_package" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runPackageMigrations requestContext
      void $ runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ stringField "version" structuredContent `shouldBe` Just netherlandsKmPackageV2.version
      assertToolOutputSchema reqHeaders "import_knowledge_model_package" structuredContent
      -- AND: Compare state in DB with expectation
      assertCountInDB (findPackagesByIdInWorkspace netherlandsKmPackageV2.id Nothing) requestContext 2
  it "HTTP 200 OK if the content is not base64 (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "import_knowledge_model_package" (object ["tenant" .= True, "content" .= ("not base64!" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_BASE64 "content")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
