module Specs.Service.Mcp.Tool.ProjectCommentTools.AddProjectComment (
  add_project_comment,
) where

import Data.Aeson
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Public
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import Shared.Util.Uuid
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (add_project_comment)
-- ------------------------------------------------------------------------
add_project_comment :: RequestContext -> SpecWith ((), Application)
add_project_comment requestContext =
  describe "POST /api/mcp (add_project_comment)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["thread_uuid" .= cmtQ1_t1.uuid, "text" .= "Agreed"]

reqBody = toolCallBody "add_project_comment" reqArguments

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
      assertToolOutputSchema reqHeaders "add_project_comment" structuredContent
      -- AND: Compare state in DB with expectation
      mThread <- getOneFromDB (findProjectCommentThreadById cmtQ1_t1.uuid) requestContext
      liftIO $ fmap (fmap (.text) . (.comments)) mThread `shouldBe` Just (fmap (.text) cmtQ1_t1.comments ++ ["Agreed"])
  createIsaacToolErrorTest
    "the thread is private and the user only comments"
    requestContext
    (runProjectMigrations requestContext >> runInContextIO (insertProjectCommentThread privateThread) requestContext)
    commentatorPermissions
    (toolCallBody "add_project_comment" (projectArguments ["thread_uuid" .= privateThread.uuid, "text" .= "Agreed"]))
    (_ERROR_DATABASE__ENTITY_NOT_FOUND "project_comment_thread" [("project_uuid", U.toString project1.uuid), ("uuid", U.toString privateThread.uuid)])
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

privateThread :: ProjectCommentThread
privateThread = cmtQ2_t1 {uuid = u' "6a8c0e2f-4b6d-4f8a-9c1e-5d7f9b1a3c46", private = True, comments = []}

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
