module Specs.Api.Handler.Workspace.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceChangeJM ()
import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Workspace.Workspace
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common

-- ------------------------------------------------------------------------
-- PUT /api/workspaces/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /api/workspaces/{uuid}" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/api/workspaces/7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = workspaceChange

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, Workspace)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareWorkspaceDtos resBody defaultWorkspaceEdited
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfWorkspaceInDB requestContext defaultWorkspaceEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "name"
  it "HTTP 400 BAD REQUEST when the name is blank" $ do
    -- GIVEN: Prepare request
    let reqBody = encode (workspaceChange {name = "   "} :: WorkspaceChangeDTO)
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = ValidationError [] (M.singleton "name" [_ERROR_VALIDATION__FIELDS_ABSENCE])
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfWorkspaceInDB requestContext defaultWorkspace

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "workspaces.manage"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/workspaces/deab6c38-aeac-4b17-a501-4365a0a70176"
    reqHeaders
    reqBody
    "workspace"
    [("uuid", "deab6c38-aeac-4b17-a501-4365a0a70176")]
