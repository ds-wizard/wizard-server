module Specs.Api.Handler.KnowledgeModelSecret.List_POST (
  list_POST,
) where

import Control.Monad (when)
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Shared.Constant.Workspace
import Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelSecret.Common
import Specs.Api.Handler.Workspace.Common

-- ------------------------------------------------------------------------
-- POST /api/knowledge-model-secrets
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/knowledge-model-secrets" $ do
    test_200 requestContext
    test_201_no_parameter requestContext
    test_201_workspace requestContext
    test_400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/knowledge-model-secrets"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = kmSecret1ChangeDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "?tenant=true") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelSecret)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareKnowledgeModelSecretDtos resBody reqDto
      liftIO $ resBody.workspaceUuid `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertExistenceOfKnowledgeModelSecretInDB requestContext reqDto

test_201_no_parameter requestContext =
  it "HTTP 201 CREATED (no parameter in a single-workspace tenant)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelSecret)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareKnowledgeModelSecretDtos resBody reqDto
      liftIO $ resBody.workspaceUuid `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertExistenceOfKnowledgeModelSecretInDB requestContext reqDto

test_201_workspace requestContext =
  it "HTTP 201 CREATED (workspace plane)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "?w=7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelSecret)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resBody.workspaceUuid `shouldBe` Just defaultWorkspaceUuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "name"
  create_test_400 "HTTP 400 BAD REQUEST - no plane in a multi-workspace tenant" requestContext True reqUrl _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED
  create_test_400 "HTTP 400 BAD REQUEST - w in a single-workspace tenant" requestContext False (reqUrl <> "?w=7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f") _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED
  create_test_400 "HTTP 400 BAD REQUEST - w with tenant=true" requestContext True (reqUrl <> "?w=7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f&tenant=true") _ERROR_SERVICE_WORKSPACE__SCOPE_CONFLICT

create_test_400 title requestContext multiWorkspace reqUrl expError =
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
