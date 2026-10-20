module Specs.Service.Mcp.Tool.ProjectCommentTools.EditProjectComment (
  edit_project_comment,
) where

import Data.Aeson
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (edit_project_comment)
-- ------------------------------------------------------------------------
edit_project_comment :: RequestContext -> SpecWith ((), Application)
edit_project_comment requestContext =
  describe "POST /api/mcp (edit_project_comment)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["thread_uuid" .= cmtQ1_t1.uuid, "comment_uuid" .= cmtQ1_t1_1.uuid, "text" .= "Edited"]

reqBody = toolCallBody "edit_project_comment" reqArguments

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
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      mThread <- getOneFromDB (findProjectCommentThreadById cmtQ1_t1.uuid) requestContext
      liftIO $ fmap (L.sort . fmap (.text) . (.comments)) mThread `shouldBe` Just (L.sort ["Edited", cmtQ1_t1_2.text])
  createIsaacToolErrorTest "the comment is of another user" requestContext (runProjectMigrations requestContext) commentatorPermissions reqBody _ERROR_SERVICE_PROJECT_COMMENT__NOT_AUTHOR
  it "HTTP 200 OK if the thread is resolved (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_PROJECT_COMMENT__THREAD_RESOLVED)
      -- AND: Run migrations
      runProjectMigrations requestContext
      _ <- request reqMethod reqUrl reqHeaders (toolCallBody "resolve_project_comment_thread" (projectArguments ["thread_uuid" .= cmtQ1_t1.uuid]))
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
