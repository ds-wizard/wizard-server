module Specs.Api.Handler.KnowledgeModelEditor.List_Suggestions_GET (
  list_suggestions_GET,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Common.WizardPageJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/knowledge-model-editors/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: RequestContext -> SpecWith ((), Application)
list_suggestions_GET requestContext =
  describe "GET /api/knowledge-model-editors/suggestions" $ do
    test_200 requestContext
    test_401 requestContext
    test_403_not_member requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/knowledge-model-editors/suggestions"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (all workspaces of the caller)"
    requestContext
    insertSecondWorkspace
    "/api/knowledge-model-editors/suggestions?sort=name,asc"
    (Page "knowledgeModelEditors" (PageMetadata 20 2 1 0) [amsterdamKnowledgeModelEditorSuggestion, secondWorkspaceKnowledgeModelEditorSuggestion])
  create_test_200
    "HTTP 200 OK (only the workspaces the caller is a member of)"
    requestContext
    insertSecondWorkspaceWithoutCaller
    "/api/knowledge-model-editors/suggestions?sort=name,asc"
    (Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) [amsterdamKnowledgeModelEditorSuggestion])

create_test_200 title requestContext prepareWorkspace reqUrl expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      prepareWorkspace requestContext
      runInContextIO (insertKnowledgeModelEditor secondWorkspaceKnowledgeModelEditor) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, Page KnowledgeModelEditorSuggestion)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto `shouldBe` expDto

secondWorkspaceKnowledgeModelEditorSuggestion :: KnowledgeModelEditorSuggestion
secondWorkspaceKnowledgeModelEditorSuggestion =
  KnowledgeModelEditorSuggestion
    { uuid = secondWorkspaceKnowledgeModelEditor.uuid
    , name = secondWorkspaceKnowledgeModelEditor.name
    }

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

test_403_not_member requestContext =
  createNotMemberTest requestContext (runInContextIO KnowledgeModelEditor.runMigration requestContext) reqMethod (BS.pack $ "/api/knowledge-model-editors/suggestions?w=" ++ U.toString secondWorkspaceUuid) reqBody "knowledgeModels.useEditor"
