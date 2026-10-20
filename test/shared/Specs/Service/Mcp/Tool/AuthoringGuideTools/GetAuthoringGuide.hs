module Specs.Service.Mcp.Tool.AuthoringGuideTools.GetAuthoringGuide (
  get_authoring_guide,
) where

import Data.Aeson
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Mcp.AuthoringGuide
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Mcp.McpGuideTopic
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (get_authoring_guide)
-- ------------------------------------------------------------------------
get_authoring_guide :: RequestContext -> SpecWith ((), Application)
get_authoring_guide requestContext =
  describe "POST /api/mcp (get_authoring_guide)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["topic" .= ("workflow" :: String)]

reqBody = toolCallBody "get_authoring_guide" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  mapM_ (create_test_200 requestContext) guideTopics
  it "HTTP 200 OK if the topic is document-template-structure (names every format step of the document worker)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_authoring_guide" (object ["topic" .= ("document-template-structure" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (guideResult "document-template-structure")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      liftIO $ filter (not . (`L.isInfixOf` guideContent "document-template-structure") . quoted) formatSteps `shouldBe` []
  it "HTTP 200 OK if the topic is template-filters (names every filter and test of the document worker)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_authoring_guide" (object ["topic" .= ("template-filters" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (guideResult "template-filters")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      liftIO $ filter (not . (`L.isInfixOf` guideContent "template-filters") . quoted) templateFilters `shouldBe` []
  it "HTTP 200 OK if the topic is knowledge-model-events (names every event type)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_authoring_guide" (object ["topic" .= ("knowledge-model-events" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (guideResult "knowledge-model-events")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      liftIO $ filter (not . (`L.isInfixOf` guideContent "knowledge-model-events") . quoted) eventTypes `shouldBe` []
  it "HTTP 200 OK if the topic is workflow (names no confirmed step without a tool)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (guideResult "workflow")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      liftIO $ guideContent "workflow" `shouldNotContain` "deleting a workspace"
  it "HTTP 200 OK if the topic is unknown (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_authoring_guide" (object ["topic" .= ("unknown" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_INVALID_ARGUMENT "topic" ("unknown topic, use one of " ++ L.intercalate ", " guideTopics))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the topic is missing (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_authoring_guide" (object [])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_MISSING_ARGUMENT "topic")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher

create_test_200 requestContext topic =
  it ("HTTP 200 OK if the topic is " ++ topic) $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_authoring_guide" (object ["topic" .= topic])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (guideResult topic)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "get_authoring_guide" (guideResult topic)

guideResult :: String -> Value
guideResult name =
  case L.find ((== name) . (.name)) authoringGuide of
    Just topic -> object ["topic" .= topic.name, "title" .= topic.title, "content" .= topic.content]
    Nothing -> Null

guideContent :: String -> String
guideContent name = maybe "" (.content) (L.find ((== name) . (.name)) authoringGuide)

quoted :: String -> String
quoted name = "`" ++ name ++ "`"

guideTopics :: [String]
guideTopics =
  [ "workflow"
  , "knowledge-model-metamodel"
  , "knowledge-model-events"
  , "document-template-structure"
  , "document-context"
  , "template-filters"
  ]

formatSteps :: [String]
formatSteps = ["json", "jinja", "pandoc", "weasyprint", "rdflib-convert", "archive", "excel", "enrich-docx"]

templateFilters :: [String]
templateFilters =
  [ "any"
  , "all"
  , "bytesize_format"
  , "datetime_format"
  , "extract"
  , "of_alphabet"
  , "roman"
  , "markdown"
  , "dot"
  , "reply_str_value"
  , "reply_int_value"
  , "reply_float_value"
  , "reply_items"
  , "find_reply"
  , "reply_path"
  , "to_context_obj"
  , "jinja2"
  , "error"
  , "extract_replies"
  , "not_empty"
  , "of_type"
  ]

eventTypes :: [String]
eventTypes =
  [ prefix ++ entity ++ "Event"
  | entity <- ["Chapter", "Question", "Answer", "Choice", "Expert", "Reference", "Tag", "Integration", "Metric", "Phase", "ResourceCollection", "ResourcePage"]
  , prefix <- ["Add", "Edit", "Delete"]
  ]
    ++ ["AddKnowledgeModelEvent", "EditKnowledgeModelEvent", "MoveQuestionEvent", "MoveAnswerEvent", "MoveChoiceEvent", "MoveExpertEvent", "MoveReferenceEvent"]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
