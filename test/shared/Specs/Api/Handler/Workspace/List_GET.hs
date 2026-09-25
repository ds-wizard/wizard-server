module Specs.Api.Handler.Workspace.List_GET (
  list_GET,
) where

import Control.Monad (when)
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Workspace.WorkspaceJM ()
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/workspaces
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/workspaces" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/workspaces"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (the only workspace)" requestContext False [reqAuthHeader] (Page "workspaces" (PageMetadata 20 1 1 0) [defaultWorkspace])
  create_test_200 "HTTP 200 OK (only the workspaces the caller is a member of)" requestContext True [reqNonAdminAuthHeader] (Page "workspaces" (PageMetadata 20 1 1 0) [defaultWorkspace])
  create_test_200 "HTTP 200 OK (both workspaces of a member)" requestContext True [reqAuthHeader] (Page "workspaces" (PageMetadata 20 2 1 0) [defaultWorkspace, secondWorkspace])

create_test_200 title requestContext withSecondWorkspace reqHeaders expDto =
  it title $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    when withSecondWorkspace (insertSecondWorkspace requestContext)
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "?sort=name,asc") reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody
