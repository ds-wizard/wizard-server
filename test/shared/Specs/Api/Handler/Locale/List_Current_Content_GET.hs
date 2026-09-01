module Specs.Api.Handler.Locale.List_Current_Content_GET (
  list_current_content_GET,
) where

import Control.Monad (void, when)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.DAO.User.UserDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC
import Shared.Database.Migration.Development.User.Data.WizardUsers
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/locales/current/content
-- ------------------------------------------------------------------------
list_current_content_GET :: RequestContext -> SpecWith ((), Application)
list_current_content_GET requestContext = describe "GET /wizard-api/locales/current/content" $ test_200 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/locales/current/content"

reqHeadersT authHeaders = authHeaders

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (anonymous)" requestContext [] "{}" False
  create_test_200 "HTTP 200 OK (authorized - use default)" requestContext [reqAuthHeader] "{}" False
  create_test_200 "HTTP 200 OK (authorized - selected NL)" requestContext [reqAuthHeader] (BSL.fromStrict localeNlContent) True

create_test_200 title requestContext authHeaders expBody changeUserLocale = do
  it title $
    -- GIVEN: Prepare variables
    do
      let reqHeaders = reqHeadersT authHeaders
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO LOC.runMigration requestContext
      runInContextIO LOC.runS3Migration requestContext
      when changeUserLocale (void $ runInContextIO (updateUserByUuid userAlbertEditedLocale) requestContext)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
