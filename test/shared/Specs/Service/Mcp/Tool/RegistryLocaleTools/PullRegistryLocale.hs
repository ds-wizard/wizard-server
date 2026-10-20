module Specs.Service.Mcp.Tool.RegistryLocaleTools.PullRegistryLocale (
  pull_registry_locale,
) where

import Data.Aeson
import qualified Data.List as L
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as Locale
import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import qualified Shared.Database.Migration.Development.MailTemplateLocale.MailTemplateLocaleMigration as MailTemplateLocale
import Shared.Database.Migration.Development.Registry.Data.RegistryLocales
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as Registry
import Shared.Model.Locale.Locale
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Locale.Common
import Specs.Api.Handler.MailTemplateLocale.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (pull_registry_locale)
-- ------------------------------------------------------------------------
pull_registry_locale :: RequestContext -> SpecWith ((), Application)
pull_registry_locale requestContext =
  describe "POST /api/mcp (pull_registry_locale)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = toolCallBody "pull_registry_locale" (object ["code" .= ("nl" :: String), "enabled" .= True])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK (a new locale is stored without confirmation)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO Locale.runMigration requestContext
      runInContextIO Registry.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "replaced" structuredContent `shouldBe` Just (Bool False)
      assertToolOutputSchema reqHeaders "pull_registry_locale" structuredContent
      -- AND: Compare state in DB with expectation
      Just locale <- getOneFromDB (findLocaleByCode' "nl") requestContext
      compareLocaleDtos locale (localeNlFromRegistry {enabled = True} :: Locale)
      assertLocaleContentInS3 requestContext locale nlRegistryLocalePo
  it "HTTP 200 OK if the confirmation token is given (the installed locale replaced)" $
    -- GIVEN: Prepare request
    do
      let reqArguments = object ["code" .= ("de" :: String)]
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO Locale.runMigration requestContext
      runInContextIO Registry.runMigration requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders (toolCallBody "pull_registry_locale" reqArguments)
      let reqBody = toolCallBody "pull_registry_locale" (object ["confirmation_token" .= token, "code" .= ("de" :: String)])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "replaced" structuredContent `shouldBe` Just (Bool True)
      -- AND: Compare state in DB with expectation
      assertExistenceOfLocaleInDB requestContext localeDeFromRegistry
      assertLocaleContentInS3 requestContext localeDe deRegistryLocalePo
  it "HTTP 200 OK if mail_templates is given and the locale is installed (summary and token, nothing stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "pull_registry_locale" (object ["code" .= ("de" :: String), "mail_templates" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO MailTemplateLocale.runMigration requestContext
      runInContextIO Registry.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ fmap (L.isInfixOf "Replaces the locale Deutsch") (stringField "summary" structuredContent) `shouldBe` Just True
      liftIO $ isJust (stringField "confirmation_token" structuredContent) `shouldBe` True
      -- AND: Compare state in DB with expectation
      assertExistenceOfMailTemplateLocaleInDB requestContext mailTemplateLocaleDe
  it "HTTP 200 OK if the registry has no such locale (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "pull_registry_locale" (object ["code" .= ("es" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO Registry.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)
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
