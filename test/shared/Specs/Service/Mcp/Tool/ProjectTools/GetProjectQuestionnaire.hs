module Specs.Service.Mcp.Tool.ProjectTools.GetProjectQuestionnaire (
  get_project_questionnaire,
) where

import Control.Monad (void)
import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Constant.Mcp
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.ProjectContent
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (get_project_questionnaire)
-- ------------------------------------------------------------------------
get_project_questionnaire :: RequestContext -> SpecWith ((), Application)
get_project_questionnaire requestContext =
  describe "POST /api/mcp (get_project_questionnaire)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments []

reqBody = toolCallBody "get_project_questionnaire" reqArguments

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
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ jsonField "replies" structuredContent `shouldBe` object ["thirdPartyContent" .= project1Ctn.replies]
      liftIO $ jsonField "phaseUuid" structuredContent `shouldBe` toJSON project1Ctn.phaseUuid
      liftIO $ jsonField "knowledgeModelPackageUuid" (jsonField "project" structuredContent) `shouldBe` toJSON germanyKmPackage.uuid
      assertToolOutputSchema reqHeaders "get_project_questionnaire" structuredContent
  it "HTTP 200 OK if the replies are larger than the output limit (continued with the cursor)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runProjectMigrations requestContext
      runInContextIO (void $ insertProjectEvents manyReplyEvents) requestContext
      expReplies <- findProjectReplies requestContext
      -- WHEN: Call API
      pages <- requestQuestionnairePages reqArguments 20
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      traverse_ (`shouldRespondWith` responseMatcher) pages
      liftIO $ length pages `shouldSatisfy` (> 1)
      liftIO $ fmap resultSizeOf pages `shouldSatisfy` all (maybe False (<= mcpMaxOutputSize))
      liftIO $ M.unions (fmap pageReplies pages) `shouldBe` fmap toJSON expReplies
      traverse_ (assertToolOutputSchema reqHeaders "get_project_questionnaire") (mapMaybe structuredContentOf pages)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
