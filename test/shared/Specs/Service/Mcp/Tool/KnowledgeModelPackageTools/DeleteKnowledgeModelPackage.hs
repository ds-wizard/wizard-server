module Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.DeleteKnowledgeModelPackage (
  delete_knowledge_model_package,
) where

import Data.Aeson
import qualified Data.List as L
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (delete_knowledge_model_package)
-- ------------------------------------------------------------------------
delete_knowledge_model_package :: RequestContext -> SpecWith ((), Application)
delete_knowledge_model_package requestContext =
  describe "POST /api/mcp (delete_knowledge_model_package)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["knowledge_model_package_uuid" .= netherlandsKmPackageV2.uuid]

reqBody = toolCallBody "delete_knowledge_model_package" reqArguments

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
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ fmap (L.isInfixOf (show (createCoordinate netherlandsKmPackageV2))) (stringField "summary" structuredContent) `shouldBe` Just True
      liftIO $ isJust (stringField "confirmation_token" structuredContent) `shouldBe` True
      assertToolOutputSchema reqHeaders "delete_knowledge_model_package" structuredContent
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelPackageInDB requestContext netherlandsKmPackageV2
  it "HTTP 200 OK if the confirmation token is given (the package deleted)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object [])
      -- AND: Run migrations
      runPackageMigrations requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "delete_knowledge_model_package" (object ["knowledge_model_package_uuid" .= netherlandsKmPackageV2.uuid, "confirmation_token" .= token])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      packageFromDb <- getOneFromDB (findPackageByUuid' netherlandsKmPackageV2.uuid) requestContext
      liftIO $ packageFromDb `shouldBe` Nothing
  it "HTTP 200 OK if all_versions is given (the summary names the id)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "delete_knowledge_model_package" (object ["knowledge_model_package_uuid" .= netherlandsKmPackageV2.uuid, "all_versions" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runPackageMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ fmap (L.isInfixOf ("all versions of the knowledge model package " ++ netherlandsKmPackageV2.id)) (stringField "summary" structuredContent) `shouldBe` Just True
  it "HTTP 200 OK if the confirmation token was issued for another package (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_MISMATCH)
      -- AND: Run migrations
      runPackageMigrations requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders (toolCallBody "delete_knowledge_model_package" (object ["knowledge_model_package_uuid" .= netherlandsKmPackage.uuid]))
      let reqBody = toolCallBody "delete_knowledge_model_package" (object ["knowledge_model_package_uuid" .= netherlandsKmPackageV2.uuid, "confirmation_token" .= token])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelPackageInDB requestContext netherlandsKmPackageV2
  createToolNoPermissionTest requestContext reqBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
