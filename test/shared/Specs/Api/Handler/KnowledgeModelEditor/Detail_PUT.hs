module Specs.Api.Handler.KnowledgeModelEditor.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Coordinate.Public
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple
import Shared.Model.User.User
import Shared.Service.KnowledgeModel.Editor.EditorService
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/knowledge-model-editors/uuid
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /wizard-api/knowledge-model-editors/uuid" $ do
    test_200 requestContext
    test_400_invalid_json requestContext
    test_400_not_valid_kmId requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/knowledge-model-editors/6474b24b-262b-42b1-9451-008e8363f2b6"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = amsterdamKnowledgeModelEditorChange

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = amsterdamKnowledgeModelEditorDetail
      -- AND: Run migrations
      runInContextIO
        ( createEditorWithParams
            amsterdamKnowledgeModelEditorList.uuid
            amsterdamKnowledgeModelEditorList.createdAt
            (fromJust requestContext.currentUser)
            amsterdamKnowledgeModelEditorCreate
        )
        requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelEditorDetailDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareEditorDtos
        resBody
        reqDto
        resBody.previousPackage
        resBody.previousPackage
        (Just userAlbert.uuid)
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfEditorInDB
        requestContext
        reqDto
        (fmap (.uuid) resBody.previousPackage)
        (fmap (.uuid) resBody.previousPackage)
        (Just userAlbert.uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json requestContext = createInvalidJsonTest reqMethod reqUrl "kmId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_not_valid_kmId requestContext =
  it "HTTP 400 BAD REQUEST when kmId is not in valid format" $
    -- GIVEN: Prepare request
    do
      let reqDto = amsterdamKnowledgeModelEditorChange {kmId = "amsterdam:km"} :: KnowledgeModelEditorChangeDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "kmId" [_ERROR_VALIDATION__INVALID_COORDINATE_PART_FORMAT "kmId" "amsterdam:km"])
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO
        ( createEditorWithParams
            amsterdamKnowledgeModelEditorList.uuid
            amsterdamKnowledgeModelEditorList.createdAt
            (fromJust requestContext.currentUser)
            amsterdamKnowledgeModelEditorCreate
        )
        requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfEditorInDB
        requestContext
        amsterdamKnowledgeModelEditorList
        amsterdamKnowledgeModelEditorList.previousPackageUuid
        amsterdamKnowledgeModelEditorList.previousPackageUuid
        (Just userAlbert.uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "KnowledgeModelEditorsUseRolePermission"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/knowledge-model-editors/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "knowledge_model_editor"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
