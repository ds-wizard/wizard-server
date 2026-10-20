module Specs.Service.Mcp.Tool.MailTemplateLocaleTools.ListMailTemplateLocales (
  list_mail_template_locales,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleJM ()
import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoJM ()
import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.MailTemplateLocale.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.MailTemplateLocaleTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_mail_template_locales)
-- ------------------------------------------------------------------------
list_mail_template_locales :: RequestContext -> SpecWith ((), Application)
list_mail_template_locales requestContext =
  describe "POST /api/mcp (list_mail_template_locales)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = toolCallBody "list_mail_template_locales" (object [])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = object ["locales" .= [mailTemplateLocaleDeDto], "pot" .= mailTemplatePotInfoDto]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runMailTemplateLocaleMigrations requestContext
      runMailTemplatePotMigration requestContext False (Just mailTemplateDefaultPot) Nothing
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "list_mail_template_locales" expContent
  createToolNoPermissionTest requestContext reqBody _SETTINGS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
