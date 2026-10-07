module Specs.Api.Handler.MailTemplateLocale.List_Pot_Info_GET (
  list_pot_info_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoJM ()
import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.MailTemplateLocale.Common

-- ------------------------------------------------------------------------
-- GET /api/mail-template-locales/pot/info
-- ------------------------------------------------------------------------
list_pot_info_GET :: RequestContext -> SpecWith ((), Application)
list_pot_info_GET requestContext =
  describe "GET /api/mail-template-locales/pot/info" $ do
    test_200 requestContext
    test_200_custom requestContext
    test_200_custom_fallback requestContext
    test_200_none requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/mail-template-locales/pot/info"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK (default POT)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = mailTemplatePotInfoDto
      let expBody = encode expDto
      -- AND: Run migrations
      runMailTemplatePotMigration requestContext False (Just mailTemplateDefaultPot) (Just mailTemplateCustomPot)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_custom requestContext =
  it "HTTP 200 OK (custom POT)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = mailTemplateCustomPotInfoDto
      let expBody = encode expDto
      -- AND: Run migrations
      runMailTemplatePotMigration requestContext True (Just mailTemplateDefaultPot) (Just mailTemplateCustomPot)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_custom_fallback requestContext =
  it "HTTP 200 OK (custom templates without a custom POT fall back to default)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = mailTemplatePotInfoDto
      let expBody = encode expDto
      -- AND: Run migrations
      runMailTemplatePotMigration requestContext True (Just mailTemplateDefaultPot) Nothing
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_none requestContext =
  it "HTTP 200 OK (no POT available yet)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = mailTemplateNonePotInfoDto
      let expBody = encode expDto
      -- AND: Run migrations
      runMailTemplatePotMigration requestContext True Nothing Nothing
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
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
