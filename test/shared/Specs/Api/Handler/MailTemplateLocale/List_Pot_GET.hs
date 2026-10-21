module Specs.Api.Handler.MailTemplateLocale.List_Pot_GET (
  list_pot_GET,
) where

import Data.Aeson (encode)
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import Shared.Localization.Messages.Locale.Public
import Shared.Model.Error.Error
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.MailTemplateLocale.Common

-- ------------------------------------------------------------------------
-- GET /api/mail-template-locales/pot
-- ------------------------------------------------------------------------
list_pot_GET :: RequestContext -> SpecWith ((), Application)
list_pot_GET requestContext =
  describe "GET /api/mail-template-locales/pot" $ do
    test_200 requestContext
    test_200_custom requestContext
    test_200_custom_fallback requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/mail-template-locales/pot"

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
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expContent = mailTemplateDefaultPot
      -- AND: Run migrations
      runMailTemplatePotMigration requestContext False (Just mailTemplateDefaultPot) (Just mailTemplateCustomPot)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, TemporaryFileDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.contentType `shouldBe` "text/x-gettext-translation-template"
      liftIO $ ("mail.pot" `L.isInfixOf` resDto.url) `shouldBe` True
      -- AND: Find result in DB and compare with expectation state
      assertTemporaryFileContentInS3 requestContext resDto expContent

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_custom requestContext =
  it "HTTP 200 OK (custom POT)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expContent = mailTemplateCustomPot
      -- AND: Run migrations
      runMailTemplatePotMigration requestContext True (Just mailTemplateDefaultPot) (Just mailTemplateCustomPot)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, TemporaryFileDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.contentType `shouldBe` "text/x-gettext-translation-template"
      liftIO $ ("mail.pot" `L.isInfixOf` resDto.url) `shouldBe` True
      -- AND: Find result in DB and compare with expectation state
      assertTemporaryFileContentInS3 requestContext resDto expContent

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_custom_fallback requestContext =
  it "HTTP 200 OK (custom templates without a custom POT fall back to default)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expContent = mailTemplateDefaultPot
      -- AND: Run migrations
      runMailTemplatePotMigration requestContext True (Just mailTemplateDefaultPot) Nothing
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, TemporaryFileDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.contentType `shouldBe` "text/x-gettext-translation-template"
      liftIO $ ("mail.pot" `L.isInfixOf` resDto.url) `shouldBe` True
      -- AND: Find result in DB and compare with expectation state
      assertTemporaryFileContentInS3 requestContext resDto expContent

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody _SETTINGS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  it "HTTP 404 NOT FOUND (no POT available yet)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = NotExistsError _ERROR_SERVICE_MAIL_TEMPLATE_LOCALE__POT_NOT_AVAILABLE
      let expBody = encode expDto
      -- AND: Run migrations
      runMailTemplatePotMigration requestContext True Nothing Nothing
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
