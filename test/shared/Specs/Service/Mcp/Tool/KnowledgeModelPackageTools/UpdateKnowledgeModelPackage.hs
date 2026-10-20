module Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.UpdateKnowledgeModelPackage (
  update_knowledge_model_package,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Common.PageMetadataJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.Library.LibraryDependentsJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (update_knowledge_model_package)
-- ------------------------------------------------------------------------
update_knowledge_model_package :: RequestContext -> SpecWith ((), Application)
update_knowledge_model_package requestContext =
  describe "POST /api/mcp (update_knowledge_model_package)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["knowledge_model_package_uuid" .= globalKmPackage.uuid, "phase" .= ("DeprecatedKnowledgeModelPackagePhase" :: String)]

reqBody = toolCallBody "update_knowledge_model_package" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (toJSON (toSimpleDTO globalKmPackageDeprecated))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "update_knowledge_model_package" (toJSON (toSimpleDTO globalKmPackageDeprecated))
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelPackageInDB requestContext (globalKmPackage {phase = DeprecatedKnowledgeModelPackagePhase} :: KnowledgeModelPackage)
  it "HTTP 200 OK if only the public flag is changed" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_knowledge_model_package" (object ["knowledge_model_package_uuid" .= globalKmPackage.uuid, "public" .= not globalKmPackage.public])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelPackageInDB requestContext (globalKmPackage {public = not globalKmPackage.public} :: KnowledgeModelPackage)
  createToolNoPermissionTest requestContext reqBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
  it "HTTP 200 OK if the package does not exist (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_knowledge_model_package" (object ["knowledge_model_package_uuid" .= unknownUuid, "public" .= False])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
