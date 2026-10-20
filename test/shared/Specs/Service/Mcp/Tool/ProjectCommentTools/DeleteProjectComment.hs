module Specs.Service.Mcp.Tool.ProjectCommentTools.DeleteProjectComment (
  delete_project_comment,
) where

import Data.Aeson
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Localization.Messages.Public
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
-- POST /api/mcp (delete_project_comment)
-- ------------------------------------------------------------------------
delete_project_comment :: RequestContext -> SpecWith ((), Application)
delete_project_comment requestContext =
  describe "POST /api/mcp (delete_project_comment)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["thread_uuid" .= cmtQ1_t1.uuid, "comment_uuid" .= cmtQ1_t1_1.uuid]

reqBody = toolCallBody "delete_project_comment" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object [])
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      mThread <- getOneFromDB (findProjectCommentThreadById cmtQ1_t1.uuid) requestContext
      liftIO $ fmap (fmap (.uuid) . (.comments)) mThread `shouldBe` Just [cmtQ1_t1_2.uuid]
  it "HTTP 200 OK if the comment is in another thread (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "delete_project_comment" (projectArguments ["thread_uuid" .= cmtQ1_t1.uuid, "comment_uuid" .= cmtQ2_t1_1.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_DATABASE__ENTITY_NOT_FOUND "project_comment" [("thread_uuid", U.toString cmtQ1_t1.uuid), ("uuid", U.toString cmtQ2_t1_1.uuid)])
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createIsaacToolErrorTest "the comment is of another user" requestContext (runProjectMigrations requestContext) commentatorPermissions reqBody _ERROR_SERVICE_PROJECT_COMMENT__NOT_AUTHOR
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
