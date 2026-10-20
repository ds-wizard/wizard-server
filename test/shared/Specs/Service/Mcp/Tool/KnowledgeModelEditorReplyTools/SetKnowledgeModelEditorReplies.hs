module Specs.Service.Mcp.Tool.KnowledgeModelEditorReplyTools.SetKnowledgeModelEditorReplies (
  set_knowledge_model_editor_replies,
) where

import Data.Aeson
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Localization.Messages.WizardPublic
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.ProjectUtil
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.KnowledgeModelEditorReplyTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (set_knowledge_model_editor_replies)
-- ------------------------------------------------------------------------
set_knowledge_model_editor_replies :: RequestContext -> SpecWith ((), Application)
set_knowledge_model_editor_replies requestContext =
  describe "POST /api/mcp (set_knowledge_model_editor_replies)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid, "replies" .= reqReplies]

reqBody = toolCallBody "set_knowledge_model_editor_replies" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["set" .= M.keys reqReplies, "cleared" .= ([] :: [String])])
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "set_knowledge_model_editor_replies" (object ["set" .= M.keys reqReplies, "cleared" .= ([] :: [String])])
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelEditorRepliesInDB requestContext amsterdamKnowledgeModelEditor.uuid reqReplies
  it "HTTP 200 OK if the call is repeated (changes nothing)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["set" .= ([] :: [String]), "cleared" .= ([] :: [String])])
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      _ <- request reqMethod reqUrl reqHeaders reqBody
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelEditorRepliesInDB requestContext amsterdamKnowledgeModelEditor.uuid reqReplies
  it "HTTP 200 OK if paths are cleared (only the replied ones, the others kept)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "set_knowledge_model_editor_replies" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid) : ["clear" .= [optionsPath, createReplyKey [chapter1.uuid, question3.uuid]]]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["set" .= ([] :: [String]), "cleared" .= [optionsPath]])
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      _ <- request reqMethod reqUrl reqHeaders (toolCallBody "set_knowledge_model_editor_replies" reqArguments)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelEditorRepliesInDB requestContext amsterdamKnowledgeModelEditor.uuid (M.fromList [(valuePath, valueReply)])
  it "HTTP 200 OK if a reply does not match its question (tool error, nothing stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "set_knowledge_model_editor_replies" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid) : ["replies" .= M.fromList [(valuePath, valueReply), (optionsPath, StringReply "Yes")]]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult ("replies[" ++ show optionsPath ++ "].value: " ++ localeMessage _ERROR_VALIDATION__PROJECT_EVENT_REPLY_TYPE_MISMATCH)
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelEditorRepliesInDB requestContext amsterdamKnowledgeModelEditor.uuid M.empty
  it "HTTP 200 OK if a path is not in the knowledge model (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "set_knowledge_model_editor_replies" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid) : ["replies" .= M.fromList [("not-a-path" :: String, valueReply)]]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult ("replies[\"not-a-path\"].path: " ++ localeMessage (_ERROR_VALIDATION__PROJECT_EVENT_PATH_ABSENCE "not-a-path"))
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

reqReplies :: M.Map String ReplyValue
reqReplies = M.fromList [(valuePath, valueReply), (optionsPath, AnswerReply q2_answerYes.uuid)]

valueReply :: ReplyValue
valueReply = StringReply "https://orcid.org/0000-0002-1825-009X"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
