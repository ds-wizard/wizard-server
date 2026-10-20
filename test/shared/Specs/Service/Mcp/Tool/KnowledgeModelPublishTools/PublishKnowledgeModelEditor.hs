module Specs.Service.Mcp.Tool.KnowledgeModelPublishTools.PublishKnowledgeModelEditor (
  publish_knowledge_model_editor,
) where

import Data.Aeson
import qualified Data.List as L
import Data.Maybe
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Config.WizardServerConfig
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Shared.Model.User.RolePermission
import Shared.S3.KnowledgeModel.KnowledgeModelLocaleS3
import Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService
import Shared.Service.KnowledgeModel.Publish.KnowledgeModelPublishService
import Shared.Service.Mcp.Tool.McpConfirmation (confirmationToken)
import Shared.Util.String (splitOn)
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Migration.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.Common
import Specs.Service.Mcp.Tool.KnowledgeModelPublishTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (publish_knowledge_model_editor)
-- ------------------------------------------------------------------------
publish_knowledge_model_editor :: RequestContext -> SpecWith ((), Application)
publish_knowledge_model_editor requestContext =
  describe "POST /api/mcp (publish_knowledge_model_editor)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid]

reqBody = toolCallBody "publish_knowledge_model_editor" reqArguments

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
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      editor <- getOneFromDB (findKnowledgeModelEditorByUuid amsterdamKnowledgeModelEditorList.uuid) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ fmap (L.isInfixOf (editor.id ++ ":" ++ editor.version)) (stringField "summary" structuredContent) `shouldBe` Just True
      liftIO $ stringField "confirmation_token" structuredContent `shouldSatisfy` isJust
      assertToolOutputSchema reqHeaders "publish_knowledge_model_editor" structuredContent
      -- AND: Compare state in DB with expectation
      assertAbsenceOfPublishedPackageInDB requestContext
      assertCountInDB (findKnowledgeModelEventsByEditorUuid editor.uuid) requestContext (length amsterdamKnowledgeModelEditorEvents)
  it "HTTP 200 OK if the confirmation token is given (the editor published)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "publish_knowledge_model_editor" (object ["confirmation_token" .= token, "knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      let Success package = fromJSON structuredContent :: Result KnowledgeModelPackageSimpleDTO
      assertToolOutputSchema reqHeaders "publish_knowledge_model_editor" structuredContent
      -- AND: Compare state in DB with expectation
      assertPublishedPackageInDB requestContext package.uuid
      assertCountInDB (findKnowledgeModelEventsByEditorUuid amsterdamKnowledgeModelEditorList.uuid) requestContext 0
  it "HTTP 200 OK if locales are given (the published package takes them over)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (insertKnowledgeModelLocale czechNetherlandsKmLocale) requestContext
      runInContextIO (putKnowledgeModelLocale czechNetherlandsKmLocale.uuid translationPoFileName czechPoContent) requestContext
      runInContextIO (putKnowledgeModelLocale czechNetherlandsKmLocale.uuid translationJsonFileName czechJsonContent) requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders (toolCallBody "publish_knowledge_model_editor" (object ["locale_uuids" .= [czechNetherlandsKmLocale.uuid], "knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid]))
      let reqBody = toolCallBody "publish_knowledge_model_editor" (object ["locale_uuids" .= [czechNetherlandsKmLocale.uuid], "confirmation_token" .= token, "knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just (Success package) = fromJSON <$> structuredContentOf response :: Maybe (Result KnowledgeModelPackageSimpleDTO)
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelLocaleCodesInDB requestContext package.uuid ["cs"]
  it "HTTP 200 OK if the editor has a completed migration (its result published)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runMigrationWithFullDB requestContext
      runInContextIO (solveAllConflicts amsterdamKnowledgeModelEditorList.uuid) requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "publish_knowledge_model_editor" (object ["confirmation_token" .= token, "knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just (Success package) = fromJSON <$> structuredContentOf response :: Maybe (Result KnowledgeModelPackageSimpleDTO)
      -- AND: Compare state in DB with expectation
      assertExistenceOfPackageUuidInDB requestContext package.uuid
      assertAbsenceOfKnowledgeModelMigrationInDB requestContext amsterdamKnowledgeModelEditorList.uuid
  it "HTTP 200 OK if the migration has conflicts (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__MIGRATION_NOT_COMPLETED)
      -- AND: Run migrations
      runMigrationWithFullDB requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the version is not higher than the latest one (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION)
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (publishPackageFromKnowledgeModelEditor packagePublishEditorDTO) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the token belongs to other arguments (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_MISMATCH)
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (insertKnowledgeModelLocale czechNetherlandsKmLocale) requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "publish_knowledge_model_editor" (object ["locale_uuids" .= [czechNetherlandsKmLocale.uuid], "confirmation_token" .= token, "knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfPublishedPackageInDB requestContext
  it "HTTP 200 OK if the token was issued before the editor changed (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_MISMATCH)
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      _ <- request reqMethod reqUrl reqHeaders (toolCallBody "save_knowledge_model_editor" (object ["uuid" .= amsterdamKnowledgeModelEditorList.uuid, "readme" .= ("# Changed" :: String)]))
      let reqBody = toolCallBody "publish_knowledge_model_editor" (object ["confirmation_token" .= token, "knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfPublishedPackageInDB requestContext
  it "HTTP 200 OK if the token is malformed (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "publish_knowledge_model_editor" (object ["confirmation_token" .= ("not-a-token" :: String), "knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_MISMATCH)
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfPublishedPackageInDB requestContext
  it "HTTP 200 OK if the token expired (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_EXPIRED)
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let [_, nonce, bindingsHash, _] = splitOn "." token
      let expiredToken = confirmationToken requestContext.serverConfig.general.secret 1000000000 nonce bindingsHash
      let reqBody = toolCallBody "publish_knowledge_model_editor" (object ["confirmation_token" .= expiredToken, "knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfPublishedPackageInDB requestContext
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
