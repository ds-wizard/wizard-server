module Specs.Service.Mcp.Tool.DocumentTemplateLocaleTools.GetDocumentTemplateTranslation (
  get_document_template_translation,
) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Localization.Messages.DocumentTemplate.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.DocumentTemplateLocaleTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (get_document_template_translation)
-- ------------------------------------------------------------------------
get_document_template_translation :: RequestContext -> SpecWith ((), Application)
get_document_template_translation requestContext =
  describe "POST /api/mcp (get_document_template_translation)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["document_template_uuid" .= wizardDocumentTemplate.uuid]

reqBody = toolCallBody "get_document_template_translation" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = object ["content" .= object ["thirdPartyContent" .= BS.unpack wizardDocumentTemplatePotContent]]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runPotFileMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "get_document_template_translation" expContent
  it "HTTP 200 OK if locale_uuid is given (the PO file of the locale)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_document_template_translation" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "locale_uuid" .= czechWizardDocumentTemplateLocale.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["content" .= object ["thirdPartyContent" .= BS.unpack czechPoContent]])
      -- AND: Run migrations
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the translation template is not generated yet (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_DOC_TML__POT_FILE_NOT_READY)
      -- AND: Run migrations
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the locale does not exist (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_document_template_translation" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "locale_uuid" .= unknownUuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)
  createToolNoPermissionTest requestContext reqBody _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
