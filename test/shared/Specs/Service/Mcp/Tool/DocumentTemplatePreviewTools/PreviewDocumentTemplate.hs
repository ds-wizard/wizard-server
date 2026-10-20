module Specs.Service.Mcp.Tool.DocumentTemplatePreviewTools.PreviewDocumentTemplate (
  preview_document_template,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Data.Aeson
import qualified Data.Text as T
import Data.Time
import Database.PostgreSQL.Simple (close, commit)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Internal (getApp, runWaiSession)

import Shared.Constant.Mcp
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftData
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplateDraft.Common (assertExistenceOfDraftDataInDB)
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.DocumentTemplatePreviewTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (preview_document_template)
-- ------------------------------------------------------------------------
preview_document_template :: RequestContext -> SpecWith ((), Application)
preview_document_template requestContext =
  describe "POST /api/mcp (preview_document_template)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = previewArguments ["project_uuid" .= project1.uuid, "format_uuid" .= formatJson.uuid]

reqBody = toolCallBody "preview_document_template" reqArguments

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
      runPreviewMigrations requestContext
      runInContextIO (updateDraftDataById emptyDraftData) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      doc <- findPreviewDocument requestContext
      let expContent = object ["state" .= ("pending" :: String), "documentUuid" .= doc.uuid]
      liftIO $ structuredContentOf response `shouldBe` Just expContent
      assertToolOutputSchema reqHeaders "preview_document_template" expContent
      -- AND: Compare state in DB with expectation
      assertPreviewDocumentInDB requestContext queuedPreview
      assertExistenceOfDraftDataInDB requestContext (wizardDocumentTemplateDraftData {projectUuid = Just project1.uuid, knowledgeModelEditorUuid = Nothing, formatUuid = Just formatJson.uuid} :: DocumentTemplateDraftData)
  it "HTTP 200 OK if a knowledge model editor is the preview source (it replaces the project)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "preview_document_template" (previewArguments ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runPreviewMigrations requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      doc <- findPreviewDocument requestContext
      liftIO $ structuredContentOf response `shouldBe` Just (object ["state" .= ("pending" :: String), "documentUuid" .= doc.uuid])
      -- AND: Compare state in DB with expectation
      assertPreviewDocumentInDB requestContext (queuedPreview {projectUuid = Nothing, knowledgeModelEditorUuid = Just amsterdamKnowledgeModelEditor.uuid} :: Document)
      assertExistenceOfDraftDataInDB requestContext (wizardDocumentTemplateDraftData {projectUuid = Nothing, knowledgeModelEditorUuid = Just amsterdamKnowledgeModelEditor.uuid} :: DocumentTemplateDraftData)
  it "HTTP 200 OK if document_uuid of a finished preview is given (its text content)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runPreviewMigrations requestContext
      documentUuid <- queuePreview requestContext
      finishPreview requestContext documentUuid "application/json" "{\"answer\":42}"
      let reqBody = toolCallBody "preview_document_template" (previewArguments ["document_uuid" .= documentUuid])
      let expContent =
            object
              [ "state" .= ("done" :: String)
              , "documentUuid" .= documentUuid
              , "fileName" .= ("preview.json" :: String)
              , "contentType" .= ("application/json" :: String)
              , "fileSize" .= (13 :: Int)
              , "content" .= object ["thirdPartyContent" .= ("{\"answer\":42}" :: String)]
              , "contentLength" .= (13 :: Int)
              ]
      let expDto = toolResult expContent
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "preview_document_template" expContent
  it "HTTP 200 OK if the text content is long (cut and continued with the cursor)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = replicate (mcpMaxOutputSize `div` 2) 'a'
      -- AND: Run migrations
      runPreviewMigrations requestContext
      documentUuid <- queuePreview requestContext
      finishPreview requestContext documentUuid "text/html" expContent
      firstResponse <- request reqMethod reqUrl reqHeaders (toolCallBody "preview_document_template" (previewArguments ["document_uuid" .= documentUuid]))
      let Just cursor = valueField "cursor" =<< structuredContentOf firstResponse
      let reqBody = toolCallBody "preview_document_template" (previewArguments ["document_uuid" .= documentUuid, "cursor" .= cursor])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let contentOf res = textOf <$> (valueField "thirdPartyContent" =<< valueField "content" =<< structuredContentOf res)
      liftIO $ (valueField "truncated" =<< structuredContentOf firstResponse) `shouldBe` Just (Bool True)
      liftIO $ (valueField "cursor" =<< structuredContentOf response) `shouldBe` Nothing
      liftIO $ ((++) <$> contentOf firstResponse <*> contentOf response) `shouldBe` Just expContent
  it "HTTP 200 OK if the finished preview is binary (only its metadata)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runPreviewMigrations requestContext
      documentUuid <- queuePreview requestContext
      finishPreview requestContext documentUuid "application/pdf" "%PDF-1.7"
      let reqBody = toolCallBody "preview_document_template" (previewArguments ["document_uuid" .= documentUuid])
      let expDto =
            toolResult $
              object
                [ "state" .= ("done" :: String)
                , "documentUuid" .= documentUuid
                , "fileName" .= ("preview.json" :: String)
                , "contentType" .= ("application/pdf" :: String)
                , "fileSize" .= (8 :: Int)
                ]
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the preview failed (the last error of the worker log with its file and line)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runPreviewMigrations requestContext
      documentUuid <- queuePreview requestContext
      failPreview requestContext documentUuid
      let reqBody = toolCallBody "preview_document_template" (previewArguments ["document_uuid" .= documentUuid])
      let expContent = object ["state" .= ("error" :: String), "documentUuid" .= documentUuid, "errorMessage" .= trimmedWorkerLog]
      let expDto = toolResult expContent
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "preview_document_template" expContent
  it "HTTP 200 OK if the preview failed and full_log is given (the full worker log)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runPreviewMigrations requestContext
      documentUuid <- queuePreview requestContext
      failPreview requestContext documentUuid
      let reqBody = toolCallBody "preview_document_template" (previewArguments ["document_uuid" .= documentUuid, "full_log" .= True])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (valueField "workerLog" =<< structuredContentOf response) `shouldBe` Just (String (T.pack workerLog))
  it "HTTP 200 OK if the tenant has too many previews being generated (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let limit = requestContext.serverConfig.mcp.documentPreviewLimit
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__PREVIEW_LIMIT limit)
      -- AND: Run migrations
      runPreviewMigrations requestContext
      insertPendingPreviews requestContext limit
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext limit
  it "HTTP 200 OK if a preview started at the same time reaches the limit (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "preview_document_template" (previewArguments [])
      -- AND: Prepare expectation
      let limit = requestContext.serverConfig.mcp.documentPreviewLimit
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__PREVIEW_LIMIT limit)
      -- AND: Run migrations
      runPreviewMigrations requestContext
      insertPendingPreviews requestContext (limit - 1)
      now <- liftIO getCurrentTime
      connection <- liftIO $ startConcurrentPreview requestContext (pendingPreview now limit)
      -- WHEN: Call API
      app <- getApp
      pendingResponse <- liftIO . async $ runWaiSession (request reqMethod reqUrl reqHeaders reqBody) app
      liftIO $ threadDelay 1000000 >> commit connection >> close connection
      response <- liftIO $ wait pendingResponse
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext limit
  it "HTTP 200 OK if a pending preview is polled over the limit" $
    -- GIVEN: Prepare expectation
    do
      let limit = requestContext.serverConfig.mcp.documentPreviewLimit
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runPreviewMigrations requestContext
      documentUuid <- queuePreview requestContext
      insertPendingPreviews requestContext limit
      let reqBody = toolCallBody "preview_document_template" (previewArguments ["document_uuid" .= documentUuid])
      let expDto = toolResult (object ["state" .= ("pending" :: String), "documentUuid" .= documentUuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if both a project and a knowledge model editor are given (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "preview_document_template" (previewArguments ["project_uuid" .= project1.uuid, "knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runPreviewMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 0
  it "HTTP 200 OK if a progress token is given (the progress streamed, then the result)" $
    -- GIVEN: Prepare request
    do
      let reqBody = previewProgressBody []
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = ["Content-Type" <:> "text/event-stream"]
      -- AND: Run migrations
      runPreviewMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      doc <- findPreviewDocument requestContext
      let messages = eventStreamMessages response
      liftIO $ fmap (valueField "method") messages `shouldBe` [Just (String "notifications/progress"), Nothing]
      liftIO $ (valueField "progressToken" =<< valueField "params" (head messages)) `shouldBe` Just (String "preview-1")
      liftIO $ last messages `shouldBe` toolResult (object ["state" .= ("pending" :: String), "documentUuid" .= doc.uuid])
  it "HTTP 200 OK if a progress token is given and the preview is finished (answered with JSON)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runPreviewMigrations requestContext
      documentUuid <- queuePreview requestContext
      finishPreview requestContext documentUuid "application/pdf" "%PDF-1.7"
      let reqBody = previewProgressBody ["document_uuid" .= documentUuid]
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (valueField "state" =<< structuredContentOf response) `shouldBe` Just (String "done")
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
