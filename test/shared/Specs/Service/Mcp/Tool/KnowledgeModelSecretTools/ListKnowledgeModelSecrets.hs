module Specs.Service.Mcp.Tool.KnowledgeModelSecretTools.ListKnowledgeModelSecrets (
  list_knowledge_model_secrets,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import Shared.Model.User.RolePermission
import Shared.Service.Mcp.Tool.KnowledgeModelSecretTools (maskKnowledgeModelSecret)
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_knowledge_model_secrets)
-- ------------------------------------------------------------------------
list_knowledge_model_secrets :: RequestContext -> SpecWith ((), Application)
list_knowledge_model_secrets requestContext =
  describe "POST /api/mcp (list_knowledge_model_secrets)" $ do
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

reqBody = toolCallBody "list_knowledge_model_secrets" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK" requestContext reqArguments [kmSecret1, workspaceKmSecret1]
  create_test_200 "HTTP 200 OK if tenant is given" requestContext (object ["tenant" .= True]) [kmSecret1]
  create_test_200 "HTTP 200 OK if workspace_uuid is given" requestContext (object ["workspace_uuid" .= defaultWorkspaceUuid]) [kmSecret1, workspaceKmSecret1]
  createToolNoPermissionTest requestContext reqBody _MCP_SENSITIVE_ROLE_PERMISSION
  createToolNoPermissionTest requestContext reqBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION

create_test_200 title requestContext reqArguments expSecrets =
  it title $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_knowledge_model_secrets" reqArguments
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = object ["secrets" .= fmap maskKnowledgeModelSecret expSecrets]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runInContextIO (insertKnowledgeModelSecret kmSecret1) requestContext
      runInContextIO (insertKnowledgeModelSecret workspaceKmSecret1) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "list_knowledge_model_secrets" expContent

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
