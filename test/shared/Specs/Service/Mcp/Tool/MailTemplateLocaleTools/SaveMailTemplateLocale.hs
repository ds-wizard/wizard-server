module Specs.Service.Mcp.Tool.MailTemplateLocaleTools.SaveMailTemplateLocale (
  save_mail_template_locale,
) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.MailTemplateLocale.MailTemplateLocaleDAO
import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.MailTemplateLocale.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.MailTemplateLocaleTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_mail_template_locale)
-- ------------------------------------------------------------------------
save_mail_template_locale :: RequestContext -> SpecWith ((), Application)
save_mail_template_locale requestContext =
  describe "POST /api/mcp (save_mail_template_locale)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = toolCallBody "save_mail_template_locale" (object ["content" .= BS.unpack mailTemplateLocaleSvPo])

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
      runMailTemplateLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "replaced" structuredContent `shouldBe` Just (Bool False)
      assertToolOutputSchema reqHeaders "save_mail_template_locale" structuredContent
      -- AND: Compare state in DB with expectation
      assertMailTemplateLocaleCodesInDB requestContext ["de", "sv"]
      Just locale <- getOneFromDB (findMailTemplateLocaleByCode' "sv") requestContext
      compareMailTemplateLocaleDtos locale mailTemplateLocaleSv
      assertMailTemplateLocaleContentInS3 requestContext locale mailTemplateLocaleSvPo
  it "HTTP 200 OK if a locale of the same code exists (summary and token, nothing stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_mail_template_locale" (object ["content" .= BS.unpack mailTemplateLocaleDeEditedPo])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runMailTemplateLocaleMigrations requestContext
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
  it "HTTP 200 OK if the confirmation token is given (the locale replaced)" $
    -- GIVEN: Prepare request
    do
      let reqArguments = object ["content" .= BS.unpack mailTemplateLocaleDeEditedPo]
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runMailTemplateLocaleMigrations requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders (toolCallBody "save_mail_template_locale" reqArguments)
      let reqBody = toolCallBody "save_mail_template_locale" (object ["confirmation_token" .= token, "content" .= BS.unpack mailTemplateLocaleDeEditedPo])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "replaced" structuredContent `shouldBe` Just (Bool True)
      -- AND: Compare state in DB with expectation
      assertExistenceOfMailTemplateLocaleInDB requestContext (mailTemplateLocaleDeEdited {defaultLocale = False} :: MailTemplateLocale)
      assertMailTemplateLocaleContentInS3 requestContext mailTemplateLocaleDe mailTemplateLocaleDeEditedPo
  it "HTTP 200 OK if only uuid and default_locale are given (the locale default)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_mail_template_locale" (object ["uuid" .= mailTemplateLocaleDe.uuid, "default_locale" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runMailTemplateLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfMailTemplateLocaleInDB requestContext mailTemplateLocaleDeDefault
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
