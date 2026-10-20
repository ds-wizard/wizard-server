module Specs.Service.Mcp.Tool.DocumentTemplateLocaleTools.SaveDocumentTemplateLocale (
  save_document_template_locale,
) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateLocaleDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Localization.Messages.DocumentTemplate.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale
import Shared.Model.User.RolePermission
import Shared.S3.DocumentTemplate.DocumentTemplateLocaleS3
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.DocumentTemplateLocaleTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_document_template_locale)
-- ------------------------------------------------------------------------
save_document_template_locale :: RequestContext -> SpecWith ((), Application)
save_document_template_locale requestContext =
  describe "POST /api/mcp (save_document_template_locale)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "content" .= BS.unpack germanPoContent]

reqBody = toolCallBody "save_document_template_locale" reqArguments

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
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ stringField "code" structuredContent `shouldBe` Just "de"
      liftIO $ stringField "name" structuredContent `shouldBe` Just "de"
      assertToolOutputSchema reqHeaders "save_document_template_locale" structuredContent
      -- AND: Compare state in DB with expectation
      assertLocaleCodesInDB requestContext ["cs", "de"]
      Just locale <- getOneFromDB (findDocumentTemplateLocaleByDocumentTemplateUuidAndCode' wizardDocumentTemplate.uuid "de") requestContext
      poContent <- getOneFromDB (retrieveDocumentTemplateLocale locale.uuid translationPoFileName) requestContext
      liftIO $ poContent `shouldBe` germanPoContent
  it "HTTP 200 OK if a locale of the same code exists (the locale replaced)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_document_template_locale" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "name" .= ("Čeština" :: String), "content" .= BS.unpack czechPoContent])
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
      let Just structuredContent = structuredContentOf response
      liftIO $ stringField "name" structuredContent `shouldBe` Just "Čeština"
      -- AND: Compare state in DB with expectation
      Just locale <- getOneFromDB (findDocumentTemplateLocaleByDocumentTemplateUuidAndCode' wizardDocumentTemplate.uuid "cs") requestContext
      liftIO $ locale.uuid `shouldNotBe` czechWizardDocumentTemplateLocale.uuid
      assertLocaleCodesInDB requestContext ["cs"]
  it "HTTP 200 OK if the PO file has no Language header (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_document_template_locale" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "content" .= BS.unpack poContentWithoutLanguage])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_VALIDATION__DOC_TML_LOCALE_MISSING_LANGUAGE)
      -- AND: Run migrations
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertLocaleCodesInDB requestContext ["cs"]
  createToolNoPermissionTest requestContext reqBody _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
