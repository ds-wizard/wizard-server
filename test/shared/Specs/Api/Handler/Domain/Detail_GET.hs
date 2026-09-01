module Specs.Api.Handler.Domain.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common ()

-- ------------------------------------------------------------------------
-- GET /wizard-api/domains
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /wizard-api/wizard-api/domains?check-domain={tenantId}" $ do
    test_204 requestContext
    test_400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT tenantId = BS.pack $ "/wizard-api/domains?check-domain=" ++ tenantId

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext = do
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT "new-domain"
      -- AND: Prepare expectation
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  create_test_400_tenant_id
    "HTTP 400 BAD REQUEST if tenantId has forbidden characters"
    requestContext
    "-forbidden-"
    (_ERROR_VALIDATION__FORBIDDEN_CHARACTERS "-forbidden-")
  create_test_400_tenant_id
    "HTTP 400 BAD REQUEST if tenantId is already used"
    requestContext
    "default"
    _ERROR_VALIDATION__TENANT_ID_UNIQUENESS

create_test_400_tenant_id title requestContext tenantId errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT tenantId
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "tenantId" [errorMessage])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
