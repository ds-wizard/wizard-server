module Specs.Api.Handler.User.List_Suggestions_GET (
  list_suggestions_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Service.User.WizardUserMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/users/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: RequestContext -> SpecWith ((), Application)
list_suggestions_GET requestContext =
  describe "GET /wizard-api/users/suggestions" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/users/suggestions"

reqHeaders = [reqNonAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (Admin)"
    requestContext
    "/wizard-api/users/suggestions?sort=uuid,asc"
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestion . toSimple) [userNikola, userIsaac, userAlbert]))
  create_test_200
    "HTTP 200 OK (Admin - pagination)"
    requestContext
    "/wizard-api/users/suggestions?sort=uuid,asc&page=1&size=1"
    (Page "users" (PageMetadata 1 3 3 1) [toSuggestion . toSimple $ userIsaac])
  create_test_200
    "HTTP 200 OK (Admin - query)"
    requestContext
    "/wizard-api/users/suggestions?sort=uuid,asc&q=te"
    (Page "users" (PageMetadata 20 2 1 0) (fmap (toSuggestion . toSimple) [userNikola, userAlbert]))
  create_test_200
    "HTTP 200 OK (Admin - select)"
    requestContext
    "/wizard-api/users/suggestions?sort=uuid,asc&select=30d48cf4-8c8a-496f-bafe-585bd238f798,e1c58e52-0824-4526-8ebe-ec38eec67030"
    (Page "users" (PageMetadata 20 2 1 0) (fmap (toSuggestion . toSimple) [userNikola, userIsaac]))
  create_test_200
    "HTTP 200 OK (Admin - exclude)"
    requestContext
    "/wizard-api/users/suggestions?sort=uuid,asc&exclude=30d48cf4-8c8a-496f-bafe-585bd238f798,e1c58e52-0824-4526-8ebe-ec38eec67030"
    (Page "users" (PageMetadata 20 1 1 0) (fmap (toSuggestion . toSimple) [userAlbert]))
  create_test_200
    "HTTP 200 OK (Admin - sort asc)"
    requestContext
    "/wizard-api/users/suggestions?sort=first_name,asc"
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestion . toSimple) [userAlbert, userIsaac, userNikola]))
  create_test_200
    "HTTP 200 OK (Admin - sort desc)"
    requestContext
    "/wizard-api/users/suggestions?sort=first_name,desc"
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestion . toSimple) [userNikola, userIsaac, userAlbert]))

create_test_200 title requestContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody
