module Specs.Service.Mcp.Tool.MailTemplateLocaleTools.GetMailTemplateTranslation (
  get_mail_template_translation,
) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import Shared.Localization.Messages.Locale.Public
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.MailTemplateLocale.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.MailTemplateLocaleTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (get_mail_template_translation)
-- ------------------------------------------------------------------------
get_mail_template_translation :: RequestContext -> SpecWith ((), Application)
get_mail_template_translation requestContext =
  describe "POST /api/mcp (get_mail_template_translation)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = toolCallBody "get_mail_template_translation" (object [])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK (the custom translation template)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = object ["content" .= object ["thirdPartyContent" .= BS.unpack mailTemplateCustomPot]]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runMailTemplatePotMigration requestContext True (Just mailTemplateDefaultPot) (Just mailTemplateCustomPot)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "get_mail_template_translation" expContent
  it "HTTP 200 OK if locale_uuid is given (the PO file of the locale)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_mail_template_translation" (object ["locale_uuid" .= mailTemplateLocaleDe.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["content" .= object ["thirdPartyContent" .= BS.unpack mailTemplateLocaleDePo]])
      -- AND: Run migrations
      runMailTemplateLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if no translation template is available (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MAIL_TEMPLATE_LOCALE__POT_NOT_AVAILABLE)
      -- AND: Run migrations
      runMailTemplatePotMigration requestContext False Nothing Nothing
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _SETTINGS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
