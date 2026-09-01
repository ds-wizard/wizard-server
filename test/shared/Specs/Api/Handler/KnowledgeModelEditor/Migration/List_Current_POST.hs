module Specs.Api.Handler.KnowledgeModelEditor.Migration.List_Current_POST (
  list_current_POST,
) where

import Data.Aeson (encode)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Tenant.Tenant
import Shared.Service.KnowledgeModel.Editor.EditorService
import qualified Shared.Service.User.WizardUserMapper as U_Mapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Migration.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/knowledge-model-editors/{uuid}/migrations/current
-- ------------------------------------------------------------------------
list_current_POST :: RequestContext -> SpecWith ((), Application)
list_current_POST requestContext =
  describe "POST /wizard-api/knowledge-model-editors/{uuid}/migrations/current" $ do
    test_201 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/knowledge-model-editors/6474b24b-262b-42b1-9451-008e8363f2b6/migrations/current"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = knowledgeModelMigrationCreateDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = knowledgeModelMigrationDTO
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithEmptyDB requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findKnowledgeModelMigrations requestContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "targetPackageUuid"
  it "HTTP 400 BAD REQUEST when migration is already created" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithFullDB requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 400 BAD REQUEST when KM editor has to have a previous package" $
    -- AND: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_VALIDATION__KM_EDITOR_PREVIOUS_PKG_ABSENCE
      let expBody = encode expDto
      -- AND: Prepare database
      let editor = amsterdamKnowledgeModelEditorCreate {previousPackageUuid = Nothing} :: KnowledgeModelEditorCreateDTO
      let editorUuid = amsterdamKnowledgeModelEditorList.uuid
      let timestamp = amsterdamKnowledgeModelEditorList.createdAt
      let user = U_Mapper.toDTO userAlbert
      runInContextIO (createEditorWithParams editorUuid timestamp user editor) requestContext
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
test_404 requestContext = do
  createNotFoundTest' reqMethod reqUrl reqHeaders reqBody "knowledge_model_editor" [("uuid", "6474b24b-262b-42b1-9451-008e8363f2b6")]
  it "HTTP 404 NOT FOUND when target previous package doesn’t exist" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            NotExistsError
              ( _ERROR_DATABASE__ENTITY_NOT_FOUND
                  "knowledge_model_package"
                  [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString netherlandsKmPackageV2.uuid)]
              )
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithEmptyDB requestContext
      runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
