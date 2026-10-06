module Specs.Api.Handler.KnowledgeModelSecret.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretMigration as KMS
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.User.Role
import Shared.Model.User.User
import Specs.Api.Handler.Workspace.Common
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/knowledge-model-secrets
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/knowledge-model-secrets" $ do
    test_200 requestContext
    test_200_workspace_role requestContext
    test_200_scope requestContext
    test_403_not_member requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/knowledge-model-secrets"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = [kmSecret1]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KMS.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_200_workspace_role requestContext =
  it "HTTP 200 OK (knowledgeModels.manage on a workspace role hides the tenant-plane secrets)" $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode [workspaceKmSecret1]
      -- AND: Run migrations
      runInContextIO KMS.runMigration requestContext
      insertSecondWorkspace requestContext
      runInContextIO (insertKnowledgeModelSecret workspaceKmSecret1) requestContext
      runInContextIO (insertKnowledgeModelSecret secondWorkspaceKmSecret) requestContext
      demoteToResearcher requestContext userAlbert
      runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userAlbert.uuid defaultWorkspaceAdminRole.uuid) requestContext
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_200_scope requestContext = do
  create_test_200_scope
    "HTTP 200 OK (tenant=true - tenant-plane secrets only)"
    requestContext
    insertSecondWorkspace
    "/api/knowledge-model-secrets?tenant=true"
    [kmSecret1]
  create_test_200_scope
    "HTTP 200 OK (w - tenant and workspace secrets)"
    requestContext
    insertSecondWorkspace
    "/api/knowledge-model-secrets?w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9"
    [kmSecret1, secondWorkspaceKmSecret]

create_test_200_scope title requestContext prepareWorkspace reqUrl expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KMS.runMigration requestContext
      prepareWorkspace requestContext
      runInContextIO (insertKnowledgeModelSecret workspaceKmSecret1) requestContext
      runInContextIO (insertKnowledgeModelSecret secondWorkspaceKmSecret) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_403_not_member requestContext =
  createNotMemberTest requestContext (runInContextIO KMS.runMigration requestContext) reqMethod "/api/knowledge-model-secrets?w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9" reqBody "knowledgeModels.manage"
