module Specs.Service.Mcp.Tool.ProjectTools.ListProjects (
  list_projects,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_projects)
-- ------------------------------------------------------------------------
list_projects :: RequestContext -> SpecWith ((), Application)
list_projects requestContext =
  describe "POST /api/mcp (list_projects)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object []

reqBody = toolCallBody "list_projects" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK" requestContext reqArguments [project3, project1, project2]
  create_test_200 "HTTP 200 OK if is_template is given" requestContext (object ["is_template" .= True]) [project1]
  create_test_200 "HTTP 200 OK if project_tags are given" requestContext (object ["project_tags" .= ["projectTag1" :: String]]) [project1, project2]
  create_test_200
    "HTTP 200 OK if knowledge_model_package_ids are given"
    requestContext
    (object ["knowledge_model_package_ids" .= [germanyKmPackage.id ++ ":" ++ germanyKmPackage.version]])
    [project3, project1, project2]
  it "HTTP 200 OK if a knowledge model package id is invalid (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_projects" (object ["knowledge_model_package_ids" .= ["nonsense" :: String]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_INVALID_ARGUMENT "knowledge_model_package_ids" "nonsense")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher

create_test_200 title requestContext reqArguments expProjects =
  it title $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_projects" reqArguments
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ projectUuidsOf response `shouldBe` fmap (Just . toJSON . (.uuid)) (expProjects :: [Project])
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "list_projects" structuredContent

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
