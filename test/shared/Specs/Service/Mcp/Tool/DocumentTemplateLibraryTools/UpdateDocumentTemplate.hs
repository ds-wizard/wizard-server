module Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.UpdateDocumentTemplate (
  update_document_template,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplate.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (update_document_template)
-- ------------------------------------------------------------------------
update_document_template :: RequestContext -> SpecWith ((), Application)
update_document_template requestContext =
  describe "POST /api/mcp (update_document_template)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "phase" .= ("DeprecatedDocumentTemplatePhase" :: String)]

reqBody = toolCallBody "update_document_template" reqArguments

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
      runTemplateMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "phase" structuredContent `shouldBe` Just (String "DeprecatedDocumentTemplatePhase")
      assertToolOutputSchema reqHeaders "update_document_template" structuredContent
      -- AND: Compare state in DB with expectation
      assertDocumentTemplatePhaseInDB requestContext wizardDocumentTemplate.uuid DeprecatedDocumentTemplatePhase
  it "HTTP 200 OK if the template is a draft (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_document_template" (object ["document_template_uuid" .= wizardDocumentTemplateDraft.uuid, "phase" .= ("ReleasedDocumentTemplatePhase" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__DRAFT_NOT_RELEASED (show wizardDocumentTemplateDraft.uuid))
      -- AND: Run migrations
      runTemplateMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertDocumentTemplatePhaseInDB requestContext wizardDocumentTemplateDraft.uuid DraftDocumentTemplatePhase
  it "HTTP 200 OK if the user has no documentTemplates.manage (tool error, nothing stored)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION)
      -- AND: Run migrations
      runTemplateMigrations requestContext
      removeAlbertPermissions requestContext [_DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION]
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertDocumentTemplatePhaseInDB requestContext wizardDocumentTemplate.uuid ReleasedDocumentTemplatePhase
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
