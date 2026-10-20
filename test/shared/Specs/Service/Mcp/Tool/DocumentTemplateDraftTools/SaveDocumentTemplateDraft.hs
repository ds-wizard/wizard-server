module Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.SaveDocumentTemplateDraft (
  save_document_template_draft,
) where

import Data.Aeson
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftData
import Shared.Model.User.RolePermission
import Shared.Util.Date
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplateDraft.Common (assertExistenceOfDraftDataInDB)
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common
import Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.Common
import Specs.Service.Mcp.Tool.DocumentTemplateFileTools.Common (assertDocumentTemplateFileNamesInDB)

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_document_template_draft)
-- ------------------------------------------------------------------------
save_document_template_draft :: RequestContext -> SpecWith ((), Application)
save_document_template_draft requestContext =
  describe "POST /api/mcp (save_document_template_draft)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["uuid" .= newDraftUuid, "name" .= ("New template" :: String), "id" .= ("org.new-template" :: String), "version" .= ("1.0.0" :: String)]

reqBody = toolCallBody "save_document_template_draft" reqArguments

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
      runInContextIO DocumentTemplate.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ jsonField "uuid" structuredContent `shouldBe` toJSON newDraftUuid
      assertToolOutputSchema reqHeaders "save_document_template_draft" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfDocumentTemplateDraftInDB requestContext newDraft
      assertExistenceOfDraftDataInDB requestContext (wizardDocumentTemplateDraftData {documentTemplateUuid = newDraftUuid, projectUuid = Nothing, formatUuid = Nothing, knowledgeModelEditorUuid = Nothing} :: DocumentTemplateDraftData)
      assertCountInDB findDrafts requestContext 2
  it "HTTP 200 OK if the call is repeated (the draft created once)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      firstResponse <- request reqMethod reqUrl reqHeaders reqBody
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ structuredContentOf response `shouldBe` structuredContentOf firstResponse
      -- AND: Compare state in DB with expectation
      assertCountInDB findDrafts requestContext 2
  it "HTTP 200 OK if the draft is based on a template with its own description" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_document_template_draft" (object ["uuid" .= newDraftUuid, "based_on" .= wizardDocumentTemplate.uuid, "name" .= ("Upgrade" :: String), "id" .= wizardDocumentTemplate.id, "version" .= ("3.0.0" :: String), "description" .= ("Upgraded" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      runInContextIO DocumentTemplate.runS3Migration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertExistenceOfDocumentTemplateDraftInDB requestContext (wizardDocumentTemplate {uuid = newDraftUuid, name = "Upgrade", version = "3.0.0", phase = DraftDocumentTemplatePhase, description = "Upgraded"})
      assertDocumentTemplateFormatNamesInDB requestContext newDraftUuid (fmap (.name) wizardDocumentTemplateFormats)
      assertDocumentTemplateFileNamesInDB requestContext newDraftUuid [fileDefaultHtml.fileName, fileDefaultCss.fileName]
  it "HTTP 200 OK if a partial change keeps the other fields and the formats" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_document_template_draft" (object ["uuid" .= wizardDocumentTemplateDraft.uuid, "readme" .= ("# Changed" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runDraftMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ jsonField "name" structuredContent `shouldBe` toJSON wizardDocumentTemplateDraft.name
      liftIO $ jsonField "readme" structuredContent `shouldBe` object ["thirdPartyContent" .= ("# Changed" :: String)]
      -- AND: Compare state in DB with expectation
      assertExistenceOfDocumentTemplateDraftInDB requestContext (wizardDocumentTemplateDraft {readme = "# Changed"})
      draft <- getOneFromDB (findDraftByUuid wizardDocumentTemplateDraft.uuid) requestContext
      liftIO $ jsonField "updatedAt" structuredContent `shouldBe` toJSON draft.updatedAt
      assertDocumentTemplateFormatsInDB requestContext wizardDocumentTemplateDraft.uuid (fmap (\format -> (format.uuid, format.name, fmap (.name) format.steps)) wizardDocumentTemplateDraftFormats)
  it "HTTP 200 OK if new formats with their steps are given" $
    -- GIVEN: Prepare request
    do
      let reqFormats = [object ["uuid" .= formatJson.uuid, "name" .= ("JSON" :: String), "icon" .= ("far fa-file" :: String), "steps" .= [object ["name" .= ("json" :: String), "options" .= object []]]]]
      let reqBody = toolCallBody "save_document_template_draft" (object ["uuid" .= wizardDocumentTemplateDraft.uuid, "formats" .= reqFormats])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runDraftMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertDocumentTemplateFormatsInDB requestContext wizardDocumentTemplateDraft.uuid [(formatJson.uuid, "JSON", ["json"])]
  it "HTTP 200 OK if the draft changed after expected_updated_at (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_document_template_draft" (object ["uuid" .= wizardDocumentTemplateDraft.uuid, "expected_updated_at" .= dt' 2018 1 1, "version" .= ("9.0.0" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__CHANGED_SINCE "document template draft" (iso8601Show wizardDocumentTemplateDraft.updatedAt))
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfDocumentTemplateDraftInDB requestContext wizardDocumentTemplateDraft
  it "HTTP 200 OK if the template is released (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_document_template_draft" (object ["uuid" .= wizardDocumentTemplate.uuid, "readme" .= ("# Changed" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__NOT_A_DRAFT (U.toString wizardDocumentTemplate.uuid))
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfDocumentTemplateInDB requestContext wizardDocumentTemplate
  it "HTTP 200 OK if a new draft has no version (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_document_template_draft" (object ["name" .= ("New template" :: String), "id" .= ("org.new-template" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__MISSING_ARGUMENTS "document template draft" ["version"])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if a new draft has no scope in a tenant with more workspaces (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_document_template_draft" (object ["name" .= ("New template" :: String), "id" .= ("org.new-template" :: String), "version" .= ("1.0.0" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED)
      -- AND: Run migrations
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if a new draft belongs to the organization (tenant: true)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_document_template_draft" (object ["uuid" .= newDraftUuid, "tenant" .= True, "name" .= ("New template" :: String), "id" .= ("org.new-template" :: String), "version" .= ("1.0.0" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertExistenceOfDocumentTemplateDraftInDB requestContext newDraft
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
