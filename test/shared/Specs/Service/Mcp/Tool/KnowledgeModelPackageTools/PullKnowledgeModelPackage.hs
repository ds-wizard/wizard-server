module Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.PullKnowledgeModelPackage (
  pull_knowledge_model_package,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Common.PageMetadataJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.Library.LibraryDependentsJM ()
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.Workspace.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (pull_knowledge_model_package)
-- ------------------------------------------------------------------------
pull_knowledge_model_package :: RequestContext -> SpecWith ((), Application)
pull_knowledge_model_package requestContext =
  describe "POST /api/mcp (pull_knowledge_model_package)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["id" .= ("org.nl.core-nl:1.0.0" :: String)]

reqBody = toolCallBody "pull_knowledge_model_package" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK if no workspace is given in a tenant with more workspaces (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED)
      -- AND: Run migrations
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
