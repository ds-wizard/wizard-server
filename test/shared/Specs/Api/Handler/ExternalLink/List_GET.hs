module Specs.Api.Handler.ExternalLink.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.ExternalLink.ExternalLinkUsageDAO
import Shared.Localization.Messages.ExternalLink.Public
import Shared.Model.Error.Error
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/external-link?url=https://docs.ds-wizard.org/my-link
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext = describe "GET /wizard-api/external-link" $ do
  test_302 requestContext
  test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_302 requestContext =
  it "HTTP 302 FOUND" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 302
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = FoundError "https://guide.ds-wizard.org/my-link"
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod "/wizard-api/external-link?url=https://guide.ds-wizard.org/my-link" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- THEN: Compare DB with expectation
      assertCountInDB findExternalLinkUsages requestContext 1

test_403 requestContext =
  it "HTTP 403 FORBIDDEN" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError _ERROR_SERVICE_EXTERNAL_LINK__URL_NOT_ALLOWED
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod "/wizard-api/external-link?url=https://evil.com" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- THEN: DB must remain empty
      assertCountInDB findExternalLinkUsages requestContext 0
