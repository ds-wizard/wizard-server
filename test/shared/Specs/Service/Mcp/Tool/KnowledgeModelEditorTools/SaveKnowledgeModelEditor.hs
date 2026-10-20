module Specs.Service.Mcp.Tool.KnowledgeModelEditorTools.SaveKnowledgeModelEditor (
  save_knowledge_model_editor,
) where

import Data.Aeson
import Data.Time.Format.ISO8601 (iso8601Show)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Common.WizardPageJM ()
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Util.Date
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common
import Specs.Service.Mcp.Tool.KnowledgeModelEditorTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_knowledge_model_editor)
-- ------------------------------------------------------------------------
save_knowledge_model_editor :: RequestContext -> SpecWith ((), Application)
save_knowledge_model_editor requestContext =
  describe "POST /api/mcp (save_knowledge_model_editor)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["uuid" .= newEditorUuid, "name" .= ("New KM" :: String), "id" .= ("org.new" :: String), "version" .= ("1.0.0" :: String)]

reqBody = toolCallBody "save_knowledge_model_editor" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "uuid" structuredContent `shouldBe` Just (toJSON newEditorUuid)
      assertToolOutputSchema reqHeaders "save_knowledge_model_editor" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfKnowledgeModelEditorInDB requestContext (amsterdamKnowledgeModelEditor {uuid = newEditorUuid, name = "New KM", id = "org.new", version = "1.0.0", description = "", readme = "", license = "", previousPackageUuid = Nothing, createdBy = Just userAlbert.uuid} :: KnowledgeModelEditor)
      assertInitialEventOfKnowledgeModelEditorInDB requestContext newEditorUuid
  it "HTTP 200 OK if the call is repeated (the editor is created once)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      firstResponse <- request reqMethod reqUrl reqHeaders reqBody
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ structuredContentOf response `shouldBe` structuredContentOf firstResponse
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelEditorsInDB requestContext [amsterdamKnowledgeModelEditor {uuid = newEditorUuid, name = "New KM", id = "org.new", version = "1.0.0", description = "", readme = "", license = "", previousPackageUuid = Nothing, createdBy = Just userAlbert.uuid} :: KnowledgeModelEditor]
  it "HTTP 200 OK if the editor forks a package with its own readme" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_knowledge_model_editor" (object ["name" .= ("Fork" :: String), "id" .= ("org.fork" :: String), "version" .= ("1.0.0" :: String), "previous_package_uuid" .= netherlandsKmPackage.uuid, "readme" .= ("# Fork" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (valueField "readme" =<< structuredContentOf response) `shouldBe` Just (object ["thirdPartyContent" .= ("# Fork" :: String)])
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelEditorsInDB requestContext [(amsterdamKnowledgeModelEditor {uuid = newEditorUuid, name = "New KM", id = "org.new", version = "1.0.0", description = "", readme = "", license = "", previousPackageUuid = Nothing, createdBy = Just userAlbert.uuid}) {name = "Fork", id = "org.fork", previousPackageUuid = Just netherlandsKmPackage.uuid, readme = "# Fork", description = netherlandsKmPackage.description, license = netherlandsKmPackage.license} :: KnowledgeModelEditor]
  it "HTTP 200 OK if a partial change keeps the other fields" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_knowledge_model_editor" (object ["uuid" .= amsterdamKnowledgeModelEditor.uuid, "readme" .= ("# Changed" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "name" structuredContent `shouldBe` Just (toJSON amsterdamKnowledgeModelEditor.name)
      -- AND: Compare state in DB with expectation
      assertExistenceOfKnowledgeModelEditorInDB requestContext (amsterdamKnowledgeModelEditor {readme = "# Changed"} :: KnowledgeModelEditor)
      editor <- getOneFromDB (findKnowledgeModelEditorByUuid amsterdamKnowledgeModelEditor.uuid) requestContext
      liftIO $ valueField "updatedAt" structuredContent `shouldBe` Just (toJSON editor.updatedAt)
  it "HTTP 200 OK if expected_updated_at is the updatedAt of the list" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      listResponse <- request reqMethod reqUrl reqHeaders (toolCallBody "list_knowledge_model_editors" (object []))
      let Just updatedAt = firstEditorUpdatedAt =<< structuredContentOf listResponse
      let reqBody = toolCallBody "save_knowledge_model_editor" (object ["uuid" .= amsterdamKnowledgeModelEditor.uuid, "expected_updated_at" .= updatedAt, "version" .= ("3.0.0" :: String)])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertExistenceOfKnowledgeModelEditorInDB requestContext (amsterdamKnowledgeModelEditor {version = "3.0.0"} :: KnowledgeModelEditor)
  it "HTTP 200 OK if the editor changed after expected_updated_at (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_knowledge_model_editor" (object ["uuid" .= amsterdamKnowledgeModelEditor.uuid, "expected_updated_at" .= dt' 2018 1 1, "version" .= ("3.0.0" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__CHANGED_SINCE "knowledge model editor" (iso8601Show amsterdamKnowledgeModelEditor.updatedAt))
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfKnowledgeModelEditorInDB requestContext amsterdamKnowledgeModelEditor
  it "HTTP 200 OK if a new editor has no name (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_knowledge_model_editor" (object ["id" .= ("org.new" :: String), "version" .= ("1.0.0" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__MISSING_ARGUMENTS "knowledge model editor" ["name"])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if a new editor has no workspace in a tenant with more workspaces (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_knowledge_model_editor" (object ["name" .= ("New KM" :: String), "id" .= ("org.new" :: String), "version" .= ("1.0.0" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED)
      -- AND: Run migrations
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
