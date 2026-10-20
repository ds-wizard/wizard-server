module Specs.Service.Mcp.Tool.KnowledgeModelLocaleTools.SaveKnowledgeModelLocale (
  save_knowledge_model_locale,
) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Either (fromRight)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import Shared.S3.KnowledgeModel.KnowledgeModelLocaleS3
import Shared.Util.Gettext
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.KnowledgeModelLocaleTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_knowledge_model_locale)
-- ------------------------------------------------------------------------
save_knowledge_model_locale :: RequestContext -> SpecWith ((), Application)
save_knowledge_model_locale requestContext =
  describe "POST /api/mcp (save_knowledge_model_locale)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

germanPoContent =
  unlines
    [ "msgid \"\""
    , "msgstr \"\""
    , "\"Language: de\\n\""
    , "\"X-Display-Name: Deutsch\\n\""
    , ""
    , "msgid \"Chapter 1\""
    , "msgstr \"Kapitel 1\""
    ]

reqArguments = object ["knowledge_model_package_uuid" .= globalKmPackage.uuid, "content" .= germanPoContent]

reqBody = toolCallBody "save_knowledge_model_locale" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
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
      liftIO $ stringField "code" structuredContent `shouldBe` Just "de"
      liftIO $ stringField "name" structuredContent `shouldBe` Just "Deutsch"
      assertToolOutputSchema reqHeaders "save_knowledge_model_locale" structuredContent
      -- AND: Compare state in DB with expectation
      assertLocaleCodesInDB requestContext ["cs", "de"]
      Just locale <- getOneFromDB (findKnowledgeModelLocaleByPackageUuidAndCode' globalKmPackage.uuid "de") requestContext
      jsonContent <- getOneFromDB (retrieveKnowledgeModelLocale locale.uuid translationJsonFileName) requestContext
      liftIO $ decodeStrict jsonContent `shouldBe` Just (toJed . fromRight [] . parsePo . BS.pack $ germanPoContent)
  it "HTTP 200 OK if a locale of the same code exists (the locale replaced)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_knowledge_model_locale" (object ["knowledge_model_package_uuid" .= globalKmPackage.uuid, "name" .= ("Čeština" :: String), "content" .= BS.unpack czechPoContent])
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
      liftIO $ stringField "name" structuredContent `shouldBe` Just "Čeština"
      -- AND: Compare state in DB with expectation
      Just locale <- getOneFromDB (findKnowledgeModelLocaleByPackageUuidAndCode' globalKmPackage.uuid "cs") requestContext
      liftIO $ locale.uuid `shouldNotBe` czechGlobalKmLocale.uuid
      assertLocaleCodesInDB requestContext ["cs"]
  it "HTTP 200 OK if the PO file has no Language header (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_knowledge_model_locale" (object ["knowledge_model_package_uuid" .= globalKmPackage.uuid, "content" .= BS.unpack poContentWithoutLanguage])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_VALIDATION__KM_LOCALE_MISSING_LANGUAGE)
      -- AND: Run migrations
      runLocaleMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertLocaleCodesInDB requestContext ["cs"]
  createToolNoPermissionTest requestContext reqBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
