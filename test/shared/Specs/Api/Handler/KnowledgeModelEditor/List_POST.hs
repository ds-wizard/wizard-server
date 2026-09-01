module Specs.Api.Handler.KnowledgeModelEditor.List_POST (
  list_POST,
) where

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
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Coordinate.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.User.User
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/knowledge-model-editors
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /wizard-api/knowledge-model-editors" $ do
    test_201 requestContext
    test_400_invalid_json requestContext
    test_400_not_valid_kmId requestContext
    test_400_not_existing_previousPackageId requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/knowledge-model-editors"

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
test_400_invalid_json requestContext = createInvalidJsonTest reqMethod reqUrl "kmId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_not_valid_kmId requestContext =
  it "HTTP 400 BAD REQUEST when kmId is not in valid format" $
    -- GIVEN: Prepare request
    do
      let reqDto = amsterdamKnowledgeModelEditorCreate {kmId = "amsterdam:km"} :: KnowledgeModelEditorCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "kmId" [_ERROR_VALIDATION__INVALID_COORDINATE_PART_FORMAT "kmId" "amsterdam:km"])
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
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "KnowledgeModelEditorsUseRolePermission"
