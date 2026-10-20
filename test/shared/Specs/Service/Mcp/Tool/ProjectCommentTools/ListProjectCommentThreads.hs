module Specs.Service.Mcp.Tool.ProjectCommentTools.ListProjectCommentThreads (
  list_project_comment_threads,
) where

import Data.Aeson
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_project_comment_threads)
-- ------------------------------------------------------------------------
list_project_comment_threads :: RequestContext -> SpecWith ((), Application)
list_project_comment_threads requestContext =
  describe "POST /api/mcp (list_project_comment_threads)" $ do
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

reqBody = toolCallBody "list_project_comment_threads" reqArguments

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
      let Just (Array threads) = valueField "commentThreads" (jsonField "_embedded" structuredContent)
      liftIO $ fmap (jsonField "uuid") (V.toList threads) `shouldMatchList` [toJSON cmtQ1_t1.uuid, toJSON cmtQ2_t1.uuid]
      assertToolOutputSchema reqHeaders "list_project_comment_threads" structuredContent
  it "HTTP 200 OK if a path is given" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_project_comment_threads" (projectArguments ["path" .= cmtQ2_path])
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
      let Just structuredContent = structuredContentOf response
      let Just (Array threads) = valueField "commentThreads" (jsonField "_embedded" structuredContent)
      liftIO $ fmap (jsonField "uuid") (V.toList threads) `shouldBe` [toJSON cmtQ2_t1.uuid]
      let comment = V.head (case jsonField "comments" (V.head threads) of Array comments -> comments; _ -> V.empty)
      liftIO $ jsonField "thirdPartyContent" (jsonField "text" comment) `shouldBe` toJSON cmtQ2_t1_1.text

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
