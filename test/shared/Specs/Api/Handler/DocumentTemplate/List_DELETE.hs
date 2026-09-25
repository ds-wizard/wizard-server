module Specs.Api.Handler.DocumentTemplate.List_DELETE (
  list_DELETE,
) where

import Data.Aeson (encode)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Localization.Messages.WizardPublic
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/document-templates
-- ------------------------------------------------------------------------
list_DELETE :: RequestContext -> SpecWith ((), Application)
list_DELETE requestContext =
  describe "DELETE /api/document-templates" $ do
    test_204 requestContext
    test_204_tenant_plane requestContext
    test_204_workspace_plane requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/api/document-templates?organizationId=global&templateId=project-report"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO DT.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDocumentTemplates requestContext 0

test_204_tenant_plane requestContext =
  it "HTTP 204 NO CONTENT (tenant=true removes only the tenant plane)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO DT.runMigration requestContext
      runInContextIO (insertDocumentTemplate workspaceDocumentTemplate) requestContext
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "&tenant=true") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDocumentTemplates requestContext 1
      templateFromDb <- getOneFromDB (findDocumentTemplateByUuid workspaceDocumentTemplate.uuid) requestContext
      liftIO $ templateFromDb.workspaceUuid `shouldBe` Just defaultWorkspaceUuid

test_204_workspace_plane requestContext =
  it "HTTP 204 NO CONTENT (w removes only the workspace plane)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO DT.runMigration requestContext
      runInContextIO (insertDocumentTemplate workspaceDocumentTemplate) requestContext
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "&w=7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDocumentTemplates requestContext 1
      eTemplate <- runInContextIO (findDocumentTemplateByUuid' workspaceDocumentTemplate.uuid) requestContext
      liftIO $ eTemplate `shouldBe` Right Nothing
      templateFromDb <- getOneFromDB (findDocumentTemplateByUuid wizardDocumentTemplate.uuid) requestContext
      liftIO $ templateFromDb.uuid `shouldBe` wizardDocumentTemplate.uuid

workspaceDocumentTemplate :: DocumentTemplate
workspaceDocumentTemplate =
  wizardDocumentTemplate
    { uuid = u' "a9b0c1d2-3e4f-4a5b-8c6d-7e8f9a0b1c2d"
    , workspaceUuid = Just defaultWorkspaceUuid
    }

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST when template can't be deleted" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCorsHeaders
      let expDto =
            UserError $
              _ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY
                (U.toString wizardDocumentTemplate.uuid)
                "project"
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO DT.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDocumentTemplates requestContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "DocumentTemplatesManageRolePermission"
