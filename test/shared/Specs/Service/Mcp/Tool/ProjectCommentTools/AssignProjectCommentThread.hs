module Specs.Service.Mcp.Tool.ProjectCommentTools.AssignProjectCommentThread (
  assign_project_comment_thread,
) where

import Data.Aeson
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (assign_project_comment_thread)
-- ------------------------------------------------------------------------
assign_project_comment_thread :: RequestContext -> SpecWith ((), Application)
assign_project_comment_thread requestContext =
  describe "POST /api/mcp (assign_project_comment_thread)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["thread_uuid" .= cmtQ1_t1.uuid, "user_uuid" .= userAlbert.uuid]

reqBody = toolCallBody "assign_project_comment_thread" reqArguments

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
      liftIO $ jsonField "uuid" (jsonField "assignedTo" structuredContent) `shouldBe` toJSON userAlbert.uuid
      -- AND: Compare state in DB with expectation
      mThread <- getOneFromDB (findProjectCommentThreadById cmtQ1_t1.uuid) requestContext
      liftIO $ fmap ((.assignedTo)) mThread `shouldBe` Just (Just userAlbert.uuid)
  it "HTTP 200 OK if the user cannot comment on the project (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "assign_project_comment_thread" (projectArguments ["thread_uuid" .= cmtQ1_t1.uuid, "user_uuid" .= userIsaac.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_PROJECT_COMMENT__ASSIGNEE_WITHOUT_ACCESS (U.toString userIsaac.uuid))
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
