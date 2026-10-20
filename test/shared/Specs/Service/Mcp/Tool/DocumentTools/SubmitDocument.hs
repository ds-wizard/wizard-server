module Specs.Service.Mcp.Tool.DocumentTools.SubmitDocument (
  submit_document,
) where

import Data.Aeson
import qualified Data.List as L
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Submission.SubmissionDAO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Settings.SettingsMigration
import Shared.Localization.Messages.Public
import Shared.Model.Settings.Settings
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.DocumentTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (submit_document)
-- ------------------------------------------------------------------------
submit_document :: RequestContext -> SpecWith ((), Application)
submit_document requestContext =
  describe "POST /api/mcp (submit_document)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = documentArguments ["service_id" .= settingsSubmissionService.sId]

reqBody = toolCallBody "submit_document" reqArguments

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
      runDocumentMigrations requestContext
      runInContextIO seedSettingsSubmissionService requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ fmap (L.isInfixOf settingsSubmissionService.name) (stringField "summary" structuredContent) `shouldBe` Just True
      liftIO $ isJust (stringField "confirmation_token" structuredContent) `shouldBe` True
      assertToolOutputSchema reqHeaders "submit_document" structuredContent
      -- AND: Compare state in DB with expectation
      assertCountInDB findSubmissions requestContext 0
  it "HTTP 200 OK if the confirmation token is given (the document submitted)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runDocumentMigrations requestContext
      runInContextIO seedSettingsSubmissionService requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "submit_document" (documentArguments ["service_id" .= settingsSubmissionService.sId, "confirmation_token" .= token])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ stringField "serviceId" structuredContent `shouldBe` Just settingsSubmissionService.sId
      assertToolOutputSchema reqHeaders "submit_document" structuredContent
      -- AND: Compare state in DB with expectation
      assertCountInDB findSubmissions requestContext 1
  it "HTTP 200 OK if the service is not available for the document (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "submit_document" (documentArguments ["service_id" .= ("otherService" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__ABSENCE "submission service")
      -- AND: Run migrations
      runDocumentMigrations requestContext
      runInContextIO seedSettingsSubmissionService requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB findSubmissions requestContext 0
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
