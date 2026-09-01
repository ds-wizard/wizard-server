module Specs.Api.Handler.Project.ProjectTag.List_Suggestions_GET (
  list_suggestions_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/projects/project-tags/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: RequestContext -> SpecWith ((), Application)
list_suggestions_GET requestContext =
  describe "GET /wizard-api/projects/project-tags/suggestions" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/projects/project-tags/suggestions"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (All)"
    requestContext
    "/wizard-api/projects/project-tags/suggestions?sort=projectTag,asc"
    ( Page
        "projectTags"
        (PageMetadata 20 4 1 0)
        [_PROJECT_TAG_1, _PROJECT_TAG_2, _SETTINGS__PROJECT_TAG_1, _SETTINGS__PROJECT_TAG_2]
    )
  create_test_200
    "HTTP 200 OK (pagination)"
    requestContext
    "/wizard-api/projects/project-tags/suggestions?sort=projectTag,asc&page=1&size=1"
    (Page "projectTags" (PageMetadata 1 4 4 1) [_PROJECT_TAG_2])
  create_test_200
    "HTTP 200 OK (query)"
    requestContext
    "/wizard-api/projects/project-tags/suggestions?sort=projectTag,asc&q=settingsProject"
    (Page "projectTags" (PageMetadata 20 2 1 0) [_SETTINGS__PROJECT_TAG_1, _SETTINGS__PROJECT_TAG_2])
  create_test_200
    "HTTP 200 OK (exclude)"
    requestContext
    "/wizard-api/projects/project-tags/suggestions?sort=projectTag,asc&exclude=settingsProjectTag2"
    ( Page
        "projectTags"
        (PageMetadata 20 3 1 0)
        [_PROJECT_TAG_1, _PROJECT_TAG_2, _SETTINGS__PROJECT_TAG_1]
    )
  create_test_200
    "HTTP 200 OK (query, exclude)"
    requestContext
    "/wizard-api/projects/project-tags/suggestions?sort=projectTag,asc&q=settings&exclude=settingsProjectTag2"
    (Page "projectTags" (PageMetadata 20 1 1 0) [_SETTINGS__PROJECT_TAG_1])

create_test_200 title requestContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody
