module Specs.Service.Mcp.Tool.LocaleTools.SaveLocale (
  save_locale,
) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Localization.Messages.Locale.Public
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Locale.Locale
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Locale.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.LocaleTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_locale)
-- ------------------------------------------------------------------------
save_locale :: RequestContext -> SpecWith ((), Application)
save_locale requestContext =
  describe "POST /api/mcp (save_locale)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = toolCallBody "save_locale" (object ["content" .= BS.unpack localeSvPo])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK (a new locale is stored disabled without confirmation)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "replaced" structuredContent `shouldBe` Just (Bool False)
      liftIO $ (stringField "code" =<< valueField "locale" structuredContent) `shouldBe` Just "sv-SE"
      assertToolOutputSchema reqHeaders "save_locale" structuredContent
      -- AND: Compare state in DB with expectation
      assertLocaleCodesInDB requestContext ["cs", "de", "sv-SE"]
      Just locale <- getOneFromDB (findLocaleByCode' "sv-SE") requestContext
      compareLocaleDtos locale localeSv
      assertLocaleContentInS3 requestContext locale localeSvPo
  it "HTTP 200 OK if default_locale is given (the new locale enabled and default)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_locale" (object ["content" .= BS.unpack localeSvPo, "default_locale" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      Just locale <- getOneFromDB (findLocaleByCode' "sv-SE") requestContext
      compareLocaleDtos locale (localeSv {enabled = True, defaultLocale = True} :: Locale)
  it "HTTP 200 OK if a locale of the same code exists (summary and token, nothing stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_locale" (object ["content" .= BS.unpack localeDeEditedPo])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ fmap (L.isInfixOf "Replaces the locale Deutsch (official, from the registry)") (stringField "summary" structuredContent) `shouldBe` Just True
      liftIO $ isJust (stringField "confirmation_token" structuredContent) `shouldBe` True
      assertToolOutputSchema reqHeaders "save_locale" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfLocaleInDB requestContext localeDe
  it "HTTP 200 OK if the confirmation token is given (the locale replaced)" $
    -- GIVEN: Prepare request
    do
      let reqArguments = object ["content" .= BS.unpack localeDeEditedPo]
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runLocaleMigrations requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders (toolCallBody "save_locale" reqArguments)
      let reqBody = toolCallBody "save_locale" (object ["confirmation_token" .= token, "content" .= BS.unpack localeDeEditedPo])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "replaced" structuredContent `shouldBe` Just (Bool True)
      -- AND: Compare state in DB with expectation
      assertExistenceOfLocaleInDB requestContext (localeDeEdited {defaultLocale = False} :: Locale)
      assertLocaleContentInS3 requestContext localeDe localeDeEditedPo
  it "HTTP 200 OK if only uuid and enabled are given (the locale enabled)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_locale" (object ["uuid" .= localeCs.uuid, "enabled" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "save_locale" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfLocaleInDB requestContext localeCsEnabled
  it "HTTP 200 OK if neither content nor uuid is given (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_locale" (object ["enabled" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__MISSING_ARGUMENTS "locale" ["content"])
      -- AND: Run migrations
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the PO file has no Language header (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_locale" (object ["content" .= BS.unpack localePoWithoutLanguage])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_VALIDATION__LOCALE_PO_MISSING_LANGUAGE)
      -- AND: Run migrations
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertLocaleCodesInDB requestContext ["cs", "de"]
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION
  createToolNoPermissionTest requestContext reqBody _SETTINGS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
