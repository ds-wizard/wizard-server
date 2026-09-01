module Specs.Api.Handler.KnowledgeModelEditor.List_GET (
  list_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Common.WizardPageJM ()
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-editors
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /wizard-api/knowledge-model-editors" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/knowledge-model-editors"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK" requestContext "/wizard-api/knowledge-model-editors" (Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) [amsterdamKnowledgeModelEditorList])
  create_test_200
    "HTTP 200 OK (query)"
    requestContext
    "/wizard-api/knowledge-model-editors?q=Amsterdam Knowledge Model"
    (Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) [amsterdamKnowledgeModelEditorList])
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    requestContext
    "/wizard-api/knowledge-model-editors?q=Non-existing KM Editor"
    (Page "knowledgeModelEditors" (PageMetadata 20 0 0 0) [])
  create_test_200_outdated "HTTP 200 OK (outdated editor)" requestContext

create_test_200 title requestContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, Page KnowledgeModelEditorList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto `shouldBe` expDto

create_test_200_outdated title requestContext =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) [amsterdamKnowledgeModelEditorList {state = OutdatedKnowledgeModelEditorState}]
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (deleteKnowledgeModelEventsByEditorUuid amsterdamKnowledgeModelEditorList.uuid) requestContext
      runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      runInContextIO (insertPackage netherlandsKmPackageV2) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, Page KnowledgeModelEditorList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto `shouldBe` expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "KnowledgeModelEditorsUseRolePermission"
