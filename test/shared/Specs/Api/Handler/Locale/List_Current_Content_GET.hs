module Specs.Api.Handler.Locale.List_Current_Content_GET (
  list_current_content_GET,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Tenant.Tenant
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Locale.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/locales/current/content
-- ------------------------------------------------------------------------
list_current_content_GET :: RequestContext -> SpecWith ((), Application)
list_current_content_GET requestContext =
  describe "GET /api/locales/current/content" $ do
    test_200 requestContext
    test_200_user_locale requestContext
    test_200_user_locale_disabled requestContext
    test_200_default requestContext
    test_200_enabled_en requestContext
    test_200_client_url requestContext
    test_200_admin_client_url requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/locales/current/content"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK (anonymous, no default)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = "{}"
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO LOC_Migration.runS3Migration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl [] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_user_locale requestContext =
  it "HTTP 200 OK (user's locale)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = BSL.fromStrict localeDeJson
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO LOC_Migration.runS3Migration requestContext
      runInContextIO (updateUserByUuid userAlbertEditedLocale) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_user_locale_disabled requestContext =
  it "HTTP 200 OK (user's locale is disabled, fall back to default)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = BSL.fromStrict localeDeJson
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO LOC_Migration.runS3Migration requestContext
      runInContextIO (updateUserByUuid userAlbertEditedLocaleCs) requestContext
      runInContextIO (updateLocaleByUuid localeDeDefault) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_default requestContext =
  it "HTTP 200 OK (anonymous, default)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = BSL.fromStrict localeDeJson
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO LOC_Migration.runS3Migration requestContext
      runInContextIO (updateLocaleByUuid localeDeDefault) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl [] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_enabled_en requestContext =
  it "HTTP 200 OK (anonymous, enabled en)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = BSL.fromStrict (toJedContent localeEnPo)
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO LOC_Migration.runS3Migration requestContext
      runLocaleEnMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl [] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_client_url requestContext =
  it "HTTP 200 OK (anonymous, clientUrl of another tenant)" $
    -- GIVEN: Prepare request
    do
      let reqUrlWithQuery = reqUrl <> BS.pack ("?clientUrl=" ++ differentTenant.clientUrl)
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = BSL.fromStrict differentLocaleJson
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO LOC_Migration.runS3Migration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrlWithQuery [] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_admin_client_url requestContext =
  it "HTTP 200 OK (anonymous, admin clientUrl of another tenant)" $
    -- GIVEN: Prepare request
    do
      let reqUrlWithQuery = reqUrl <> BS.pack ("?clientUrl=" ++ differentTenant.clientUrl ++ "/admin")
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = BSL.fromStrict differentLocaleJson
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO LOC_Migration.runS3Migration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrlWithQuery [] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
