module Specs.Api.Handler.UserGroup.List_Suggestions_GET (
  list_suggestions_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.Migration.Development.User.Data.UserGroups
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import WizardServer.Model.Context.RequestContext
import WizardServer.Service.User.Group.UserGroupMapper

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/user-groups/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: RequestContext -> SpecWith ((), Application)
list_suggestions_GET requestContext =
  describe "GET /wizard-api/user-groups/suggestions" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/user-groups/suggestions"

reqHeaders = [reqNonAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    requestContext
    "/wizard-api/user-groups/suggestions?sort=uuid,asc"
    (Page "userGroups" (PageMetadata 20 2 1 0) [toSuggestion bioGroup, toSuggestion plantGroup])
  create_test_200
    "HTTP 200 OK (pagination)"
    requestContext
    "/wizard-api/user-groups/suggestions?sort=uuid,asc&page=1&size=1"
    (Page "userGroups" (PageMetadata 1 2 2 1) [toSuggestion plantGroup])
  create_test_200
    "HTTP 200 OK (query)"
    requestContext
    "/wizard-api/user-groups/suggestions?sort=uuid,asc&q=plan"
    (Page "userGroups" (PageMetadata 20 1 1 0) [toSuggestion plantGroup])
  create_test_200
    "HTTP 200 OK (sort asc)"
    requestContext
    "/wizard-api/user-groups/suggestions?sort=name,asc"
    (Page "userGroups" (PageMetadata 20 2 1 0) [toSuggestion bioGroup, toSuggestion plantGroup])
  create_test_200
    "HTTP 200 OK (sort desc)"
    requestContext
    "/wizard-api/user-groups/suggestions?sort=name,desc"
    (Page "userGroups" (PageMetadata 20 2 1 0) [toSuggestion plantGroup, toSuggestion bioGroup])

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
