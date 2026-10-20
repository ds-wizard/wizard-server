module Specs.Service.Mcp.Tool.RegistryLocaleTools.ListRegistryLocales (
  list_registry_locales,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Locale.Registry.RegistryLocaleJM ()
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as Locale
import qualified Shared.Database.Migration.Development.MailTemplateLocale.MailTemplateLocaleMigration as MailTemplateLocale
import Shared.Database.Migration.Development.Registry.Data.RegistryLocales
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as Registry
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_registry_locales)
-- ------------------------------------------------------------------------
list_registry_locales :: RequestContext -> SpecWith ((), Application)
list_registry_locales requestContext =
  describe "POST /api/mcp (list_registry_locales)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = toolCallBody "list_registry_locales" (object [])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = object ["locales" .= [deRegistryLocaleDto, nlRegistryLocaleDto]]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runInContextIO Locale.runMigration requestContext
      runInContextIO Registry.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "list_registry_locales" expContent
  it "HTTP 200 OK if mail_templates is given (the mail template locales)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_registry_locales" (object ["mail_templates" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["locales" .= [deRegistryMailTemplateLocaleDto]])
      -- AND: Run migrations
      runInContextIO MailTemplateLocale.runMigration requestContext
      runInContextIO Registry.runMigration requestContext
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
