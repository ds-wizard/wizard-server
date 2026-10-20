module Specs.Service.Mcp.Tool.KnowledgeModelEventTools.DescribeKnowledgeModelEvents (
  describe_knowledge_model_events,
) where

import Control.Monad
import Data.Aeson
import Data.Foldable (traverse_)
import Data.JSON.JSONSchema (validate)
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeJM ()
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Service.Mcp.Tool.KnowledgeModelEventTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (describe_knowledge_model_events)
-- ------------------------------------------------------------------------
describe_knowledge_model_events :: RequestContext -> SpecWith ((), Application)
describe_knowledge_model_events requestContext =
  describe "POST /api/mcp (describe_knowledge_model_events)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["types" .= ["AddChapterEvent" :: String, "EditTagEvent"]]

reqBody = toolCallBody "describe_knowledge_model_events" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      let Just descriptions = arrayOf =<< valueField "events" structuredContent
      liftIO $ fmap (valueField "eventType") descriptions `shouldBe` [Just (String "AddChapterEvent"), Just (String "EditTagEvent")]
      liftIO $ fmap (valueField "eventType" <=< valueField "content" <=< valueField "example") descriptions `shouldBe` [Just (String "AddChapterEvent"), Just (String "EditTagEvent")]
      assertToolOutputSchema reqHeaders "describe_knowledge_model_events" structuredContent
  it "HTTP 200 OK if every event type of edit_knowledge_model is described (its example satisfies its schema)" $
    -- GIVEN: Prepare request
    do
      let reqBody = rpcBody "tools/list" (object [])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let eventTypes = editEventTypesOf response
      responses <- traverse (\eventType -> request reqMethod reqUrl reqHeaders (toolCallBody "describe_knowledge_model_events" (object ["types" .= [eventType]]))) eventTypes
      let descriptions = concatMap (\eventResponse -> fromMaybe [] (arrayOf =<< valueField "events" =<< structuredContentOf eventResponse)) responses
      liftIO $ length eventTypes `shouldBe` 43
      liftIO $ fmap (valueField "eventType") descriptions `shouldBe` fmap (Just . String . T.pack) eventTypes
      liftIO $ traverse_ assertExampleSatisfiesSchema descriptions
  it "HTTP 200 OK if an event type is unknown (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "describe_knowledge_model_events" (object ["types" .= ["AddUnicornEvent" :: String]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)

editEventTypesOf response =
  let tool = L.find ((== Just (String "edit_knowledge_model")) . valueField "name") (toolsOf response)
      path = ["inputSchema", "properties", "events", "items", "properties", "content", "properties", "eventType", "enum"]
   in case foldl (\value key -> valueField key =<< value) tool path of
        Just (Array names) -> [T.unpack name | String name <- V.toList names]
        _ -> []

assertExampleSatisfiesSchema description = do
  let Just schema = valueField "schema" description
  let Just content = valueField "content" =<< valueField "example" description
  (valueField "eventType" description, validate schema content) `shouldBe` (valueField "eventType" description, Right ())

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
