module Specs.Api.Handler.KnowledgeModelEditor.Migration.List_Current_Conflict_POST (
  list_current_conflict_POST,
) where

import Data.Aeson (encode)
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Migration.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/knowledge-model-editors/uuid/migrations/current/conflict
-- ------------------------------------------------------------------------
list_current_conflict_POST :: RequestContext -> SpecWith ((), Application)
list_current_conflict_POST requestContext =
  describe "POST /wizard-api/knowledge-model-editors/{uuid}/migrations/current/conflict" $ do
    test_204 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/knowledge-model-editors/6474b24b-262b-42b1-9451-008e8363f2b6/migrations/current/conflict"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = knowledgeModelMigrationResolutionDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = ""
      -- AND: Prepare database
      runMigrationWithFullDB requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertStateOfMigrationInDB requestContext knowledgeModelMigrationDTO CompletedKnowledgeModelMigrationState

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "targetPackageUuid"
  it "HTTP 400 BAD REQUEST when originalEventUuid doesn't match with current target event" $
    -- GIVEN: Prepare request
    do
      let reqDtoEdited = reqDto {originalEventUuid = fromJust . U.fromString $ "30ac5193-5685-41b1-86d7-ab0b356c516a"}
      let reqBody = encode reqDtoEdited
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_SERVICE_MIGRATION_KM__EVENT_UUIDS_MISMATCH
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithFullDB requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 400 BAD REQUEST when you can't solve conflicts because Migration state isn't in conflict state" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_SERVICE_MIGRATION_KM__NO_CONFLICTS_TO_SOLVE
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithFullDB requestContext
      runInContextIO (solveConflictAndMigrate amsterdamKnowledgeModelEditorList.uuid reqDto) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

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
    reqUrl
    reqHeaders
    reqBody
    "knowledge_model_migration"
    [("editor_uuid", "6474b24b-262b-42b1-9451-008e8363f2b6")]
