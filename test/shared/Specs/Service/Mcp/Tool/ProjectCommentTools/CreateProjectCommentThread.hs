module Specs.Service.Mcp.Tool.ProjectCommentTools.CreateProjectCommentThread (
  create_project_comment_thread,
) where

import Data.Aeson
import Data.Maybe (fromJust)
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
-- POST /api/mcp (create_project_comment_thread)
-- ------------------------------------------------------------------------
create_project_comment_thread :: RequestContext -> SpecWith ((), Application)
create_project_comment_thread requestContext =
  describe "POST /api/mcp (create_project_comment_thread)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["path" .= cmtQ2_path, "text" .= "Please check"]

reqBody = toolCallBody "create_project_comment_thread" reqArguments

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
      liftIO $ jsonField "path" structuredContent `shouldBe` toJSON cmtQ2_path
      assertToolOutputSchema reqHeaders "create_project_comment_thread" structuredContent
      -- AND: Compare state in DB with expectation
      let threadUuid = fromJust (U.fromText =<< (\case String text -> Just text; _ -> Nothing) (jsonField "uuid" structuredContent))
      mThread <- getOneFromDB (findProjectCommentThreadById threadUuid) requestContext
      liftIO $ fmap (\thread -> (thread.path, fmap (.text) thread.comments)) mThread `shouldBe` Just (cmtQ2_path, ["Please check"])
  it "HTTP 200 OK if the path is not a question (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "create_project_comment_thread" (projectArguments ["path" .= "unknown", "text" .= "Please check"])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult ("path: " ++ localeMessage (_ERROR_VALIDATION__PROJECT_EVENT_PATH_ABSENCE "unknown"))
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createIsaacToolErrorTest
    "the thread is private and the user only comments"
    requestContext
    (runProjectMigrations requestContext)
    commentatorPermissions
    (toolCallBody "create_project_comment_thread" (projectArguments ["path" .= cmtQ2_path, "text" .= "Please check", "private" .= True]))
    (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
