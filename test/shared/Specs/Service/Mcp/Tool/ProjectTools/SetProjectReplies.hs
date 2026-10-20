module Specs.Service.Mcp.Tool.ProjectTools.SetProjectReplies (
  set_project_replies,
) where

import Data.Aeson
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.WizardPublic
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.ProjectContent
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.ProjectUtil
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (set_project_replies)
-- ------------------------------------------------------------------------
set_project_replies :: RequestContext -> SpecWith ((), Application)
set_project_replies requestContext =
  describe "POST /api/mcp (set_project_replies)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqReplies = M.fromList [(fst rQ1, StringReply "https://orcid.org/0000-0002-1825-009X"), (fst rQ2, (snd rQ2).value)]

reqArguments = projectArguments ["replies" .= reqReplies]

reqBody = toolCallBody "set_project_replies" reqArguments

itemPath = createReplyKey [chapter2.uuid, question4.uuid, newItemUuid, q4_it1_question5.uuid]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["set" .= [fst rQ1], "cleared" .= ([] :: [String])])
      let expReplies = M.union reqReplies (fmap (.value) project1Ctn.replies)
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertProjectRepliesInDB requestContext expReplies
  it "HTTP 200 OK if the call is repeated (nothing set)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["set" .= ([] :: [String]), "cleared" .= ([] :: [String])])
      let expReplies = M.union reqReplies (fmap (.value) project1Ctn.replies)
      -- AND: Run migrations
      runProjectMigrations requestContext
      _ <- request reqMethod reqUrl reqHeaders reqBody
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertProjectRepliesInDB requestContext expReplies
  it "HTTP 200 OK if paths to clear are given (only the replied ones cleared)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "set_project_replies" (projectArguments ["clear" .= [fst rQ2, itemPath]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["set" .= ([] :: [String]), "cleared" .= [fst rQ2]])
      let expReplies = M.delete (fst rQ2) (fmap (.value) project1Ctn.replies)
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertProjectRepliesInDB requestContext expReplies
  it "HTTP 200 OK if items of a list question are given" $
    -- GIVEN: Prepare request
    do
      let reqItemReplies = M.fromList [(fst rQ4, ItemListReply [newItemUuid]), (itemPath, ItemListReply [])]
      let reqBody = toolCallBody "set_project_replies" (projectArguments ["replies" .= reqItemReplies])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expReplies = M.union reqItemReplies (fmap (.value) project1Ctn.replies)
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
      assertProjectRepliesInDB requestContext expReplies
  it "HTTP 200 OK if a reply does not match its question (tool error, nothing stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "set_project_replies" (projectArguments ["replies" .= M.insert (fst rQ2) (StringReply "Yes") (fmap (.value) project1Ctn.replies)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult ("replies[" ++ show (fst rQ2) ++ "].value: " ++ localeMessage _ERROR_VALIDATION__PROJECT_EVENT_REPLY_TYPE_MISMATCH)
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertProjectRepliesInDB requestContext (fmap (.value) project1Ctn.replies)
  it "HTTP 200 OK if an item is not in its list (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "set_project_replies" (projectArguments ["replies" .= M.fromList [(itemPath, ItemListReply [])]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult ("replies[" ++ show itemPath ++ "].path: " ++ localeMessage (_ERROR_VALIDATION__PROJECT_EVENT_ITEM_ABSENCE newItemUuid))
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
