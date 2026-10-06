module Specs.Api.Handler.KnowledgeModelEditor.List_POST (
  list_POST,
) where

import Control.Monad (when)
import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Coordinate.Public
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/knowledge-model-editors
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/knowledge-model-editors" $ do
    test_201 requestContext
    test_201_workspace requestContext
    test_400_invalid_json requestContext
    test_400_not_valid_id requestContext
    test_400_not_existing_previousPackageId requestContext
    test_400_workspace requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext
    test_404_workspace requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/knowledge-model-editors"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = amsterdamKnowledgeModelEditorCreate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = amsterdamKnowledgeModelEditorDetail
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareKnowledgeModelEditor
        resBody
        reqDto
        reqDto.previousPackageUuid
        reqDto.previousPackageUuid
        (Just userAlbert.uuid)
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findKnowledgeModelEditors requestContext 1
      assertExistenceOfEditorInDB
        requestContext
        reqDto
        reqDto.previousPackageUuid
        reqDto.previousPackageUuid
        (Just userAlbert.uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_workspace requestContext =
  it "HTTP 201 CREATED (multi-workspace tenant with w)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      enableMultiWorkspace requestContext
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "?w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resBody.workspaceUuid `shouldBe` secondWorkspace.uuid
      -- AND: Find result in DB and compare with expectation state
      editorInDB <- getOneFromDB (findKnowledgeModelEditorByUuid resBody.uuid) requestContext
      liftIO $ editorInDB.workspaceUuid `shouldBe` secondWorkspace.uuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json requestContext = createInvalidJsonTest reqMethod reqUrl "id"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_not_valid_id requestContext =
  it "HTTP 400 BAD REQUEST when id is not in valid format" $
    -- GIVEN: Prepare request
    do
      let reqDto = amsterdamKnowledgeModelEditorCreate {id = "amsterdam:km"} :: KnowledgeModelEditorCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "id" [_ERROR_VALIDATION__INVALID_COORDINATE_PART_FORMAT "id" "amsterdam:km"])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findKnowledgeModelEditors requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_not_existing_previousPackageId requestContext =
  it "HTTP 400 BAD REQUEST when previousPackageUuid does not exist" $
    -- GIVEN: Prepare request
    do
      let reqDto = amsterdamKnowledgeModelEditorCreate {previousPackageUuid = Just . u' $ "38c6a9e3-0398-4260-b34f-2d0a784023be"} :: KnowledgeModelEditorCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "previousPackageUuid" [_ERROR_VALIDATION__PREVIOUS_PKG_ABSENCE])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findKnowledgeModelEditors requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_workspace requestContext = do
  create_test_400_workspace "HTTP 400 BAD REQUEST - missing w in a multi-workspace tenant" requestContext True reqUrl _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED
  create_test_400_workspace "HTTP 400 BAD REQUEST - w in a single-workspace tenant" requestContext False (reqUrl <> "?w=7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f") _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED

create_test_400_workspace title requestContext multiWorkspace reqUrl expError =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (UserError expError)
      -- AND: Run migrations
      when multiWorkspace (enableMultiWorkspace requestContext)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findKnowledgeModelEditors requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "knowledgeModels.useEditor"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  it "HTTP 404 NOT FOUND when previousPackageUuid belongs to another workspace" $
    -- GIVEN: Prepare request
    do
      let reqDto = amsterdamKnowledgeModelEditorCreate {previousPackageUuid = Just secondWorkspaceKmPackage.uuid} :: KnowledgeModelEditorCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = NotExistsError (_ERROR_VALIDATION__ABSENCE "knowledge_model_package")
      let expBody = encode expDto
      -- AND: Run migrations
      enableMultiWorkspace requestContext
      insertSecondWorkspace requestContext
      runInContextIO (insertPackage secondWorkspaceKmPackage) requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "?w=7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findKnowledgeModelEditors requestContext 0

test_404_workspace requestContext =
  it "HTTP 404 NOT FOUND when the caller is not a member of w" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = NotExistsError (_ERROR_VALIDATION__ABSENCE "workspace")
      let expBody = encode expDto
      -- AND: Run migrations
      insertSecondWorkspaceWithoutCaller requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "?w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findKnowledgeModelEditors requestContext 0

secondWorkspaceKmPackage :: KnowledgeModelPackage
secondWorkspaceKmPackage =
  netherlandsKmPackage
    { uuid = u' "9c8b7a6f-5e4d-4c3b-a2f1-0e9d8c7b6a5f"
    , workspaceUuid = Just secondWorkspace.uuid
    }
