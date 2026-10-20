module Specs.Service.Mcp.Tool.KnowledgeModelEditorTools.ListKnowledgeModelEditors (
  list_knowledge_model_editors,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Common.WizardPageJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_knowledge_model_editors)
-- ------------------------------------------------------------------------
list_knowledge_model_editors :: RequestContext -> SpecWith ((), Application)
list_knowledge_model_editors requestContext =
  describe "POST /api/mcp (list_knowledge_model_editors)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object []

reqBody = toolCallBody "list_knowledge_model_editors" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (toJSON (Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) ([amsterdamKnowledgeModelEditorList] :: [KnowledgeModelEditorList])))
      -- AND: Run migrations
      runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "list_knowledge_model_editors" structuredContent
  it "HTTP 200 OK if the editor is outdated and has no unpublished changes" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_knowledge_model_editors" (object ["q" .= ("Amsterdam" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (toJSON (Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) ([amsterdamKnowledgeModelEditorList {state = OutdatedKnowledgeModelEditorState}] :: [KnowledgeModelEditorList])))
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (deleteKnowledgeModelEventsByEditorUuid amsterdamKnowledgeModelEditorList.uuid) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the editors are listed for a workspace" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_knowledge_model_editors" (object ["workspace_uuid" .= defaultWorkspaceUuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (toJSON (Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) ([amsterdamKnowledgeModelEditorList] :: [KnowledgeModelEditorList])))
      -- AND: Run migrations
      runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the workspace has no editors" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_knowledge_model_editors" (object ["workspace_uuid" .= secondWorkspaceUuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (toJSON (Page "knowledgeModelEditors" (PageMetadata 20 0 0 0) ([] :: [KnowledgeModelEditorList])))
      -- AND: Run migrations
      runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
  it "HTTP 200 OK if the toolsets leave km out (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = "/api/mcp?toolsets=dt"
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__TOOLSET_NOT_ENABLED "km")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
