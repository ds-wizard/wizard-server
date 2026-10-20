module Specs.Service.Mcp.Tool.KnowledgeModelEventTools.EditKnowledgeModel (
  edit_knowledge_model,
) where

import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeJM ()
import Shared.Constant.Mcp
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Mcp.McpCursor
import Shared.Model.Tenant.Tenant
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Editor.EditorMapper (toKnowledgeModelEditorEvent)
import Shared.Service.Mcp.Server.McpOutputLimit (encodeCursor, truncatedErrorSuffix)
import Shared.Util.Date
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Event.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.KnowledgeModelEventTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (edit_knowledge_model)
-- ------------------------------------------------------------------------
edit_knowledge_model :: RequestContext -> SpecWith ((), Application)
edit_knowledge_model requestContext =
  describe "POST /api/mcp (edit_knowledge_model)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid, "events" .= symbolicNameEventChanges]

reqBody = toolCallBody "edit_knowledge_model" reqArguments

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
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      let Just (Success names) = fromJSON <$> valueField "names" structuredContent
      let Just (Success revision) = fromJSON <$> valueField "revision" structuredContent
      liftIO $ M.keys names `shouldBe` ["$chapter", "$question"]
      let Just outline = outlineOf structuredContent
      liftIO $ [valueField "uuid" node | node <- outline, valueField "depth" node == Just (Number 0)] `shouldBe` [Just (toJSON (names M.! "$chapter"))]
      liftIO $ [valueField "parentUuid" node | node <- outline, valueField "uuid" node == Just (toJSON (names M.! "$question"))] `shouldBe` [Just (toJSON (names M.! "$chapter"))]
      assertToolOutputSchema reqHeaders "edit_knowledge_model" structuredContent
      -- AND: Compare state in DB with expectation
      assertAppendedEventsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents (symbolicNameEvents names) revision
  it "HTTP 200 OK if the batch is a dry run (nothing stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "edit_knowledge_model" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : ["events" .= symbolicNameEventChanges, "dry_run" .= True]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (valueField "revision" =<< structuredContentOf response) `shouldBe` Just (toJSON amsterdamRevision)
      -- AND: Compare state in DB with expectation
      assertEventUuidsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents
  it "HTTP 200 OK if events are invalid (tool error listing every one)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "edit_knowledge_model" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : ["events" .= [eventChange "$missing" "$chapter" (toJSON a_km1_ch4.content), eventChange (U.toString km1.uuid) "not-a-uuid" (toJSON a_km1_ch4.content)]]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            toolErrorResult $
              "events[0].parentUuid: "
                ++ localeMessage (_ERROR_VALIDATION__KM_EVENT_UNDEFINED_NAME "$missing")
                ++ "; events[1].entityUuid: "
                ++ localeMessage (_ERROR_VALIDATION__KM_EVENT_INVALID_UUID "not-a-uuid")
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertEventUuidsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents
  it "HTTP 200 OK if the base revision is outdated (tool error with the events added since)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "edit_knowledge_model" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : ["events" .= symbolicNameEventChanges, "base_revision" .= (head amsterdamKnowledgeModelEditorEvents).uuid]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expMessage = localeMessage (_ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__OUTDATED_BASE_REVISION (U.toString (head amsterdamKnowledgeModelEditorEvents).uuid))
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)
      liftIO $ fmap (L.isPrefixOf expMessage) (errorTextOf response) `shouldBe` Just True
      liftIO $ fmap (L.isInfixOf (U.toString amsterdamRevision)) (errorTextOf response) `shouldBe` Just True
      -- AND: Compare state in DB with expectation
      assertEventUuidsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents
  it "HTTP 200 OK if the events added since the base revision exceed the output limit (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "edit_knowledge_model" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : ["events" .= symbolicNameEventChanges, "base_revision" .= amsterdamRevision]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (traverse_ (insertKnowledgeModelEvent . toKnowledgeModelEditorEvent amsterdamKnowledgeModelEditorList.uuid defaultTenant.uuid) addedEvents) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)
      liftIO $ resultSizeOf response `shouldSatisfy` maybe False (<= mcpMaxOutputSize)
      liftIO $ fmap (L.isSuffixOf truncatedErrorSuffix) (errorTextOf response) `shouldBe` Just True
  it "HTTP 200 OK if the editor has a migration (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult . localeMessage $ _ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__MIGRATION_IN_PROGRESS
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runEditorMigrationMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertEventUuidsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents
  it "HTTP 200 OK if the result is truncated (no cursor, the batch stored once)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "edit_knowledge_model" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : ["events" .= manyTagEvents]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "truncated" structuredContent `shouldBe` Just (Bool True)
      liftIO $ valueField "cursor" structuredContent `shouldSatisfy` isNothing
      cursorResponse <- request reqMethod reqUrl reqHeaders (toolCallBody "edit_knowledge_model" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : ["events" .= manyTagEvents, "cursor" .= encodeCursor (McpCursor [KeyMcpCursorStep "outline", KeyMcpCursorStep "thirdPartyContent"] 10)])))
      liftIO $ toolErrorOf cursorResponse `shouldBe` Just (Bool True)
      -- AND: Compare state in DB with expectation
      assertCountInDB (findKnowledgeModelEventsByEditorUuid amsterdamKnowledgeModelEditorList.uuid) requestContext (length amsterdamKnowledgeModelEditorEvents + 500)
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

addedEvents :: [KnowledgeModelEvent]
addedEvents = fmap (\index -> (a_km1_tds {uuid = U.fromWords index 1 2 3, entityUuid = U.fromWords index 4 5 6, createdAt = dt' 2100 1 1} :: KnowledgeModelEvent)) [1 .. 300]

manyTagEvents = fmap (\index -> eventChange (U.toString km1.uuid) ("$tag" ++ show index) (toJSON a_km1_tds.content)) [1 .. 500 :: Int]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
