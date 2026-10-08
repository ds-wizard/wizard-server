module Specs.Api.Handler.Tenant.Usage.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Tenant.Usage.WizardUsageJM ()
import Shared.Database.Migration.Development.Tenant.Data.TenantUsages
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- GET /api/tenants/{uuid}/usages
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /api/tenants/{uuid}/usages" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/tenants/00000000-0000-0000-0000-000000000000/usages"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = defaultUsage
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody _TENANTS_MANAGE_ROLE_PERMISSION
