module Specs.Api.Handler.KnowledgeModelPackage.List_Suggestions_GET (
  list_suggestions_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-packages/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: RequestContext -> SpecWith ((), Application)
list_suggestions_GET requestContext =
  describe "GET /wizard-api/knowledge-model-packages/suggestions" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/knowledge-model-packages/suggestions"

reqHeaders = [reqNonAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    requestContext
    "/wizard-api/knowledge-model-packages/suggestions?sort=organizationId,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 3 1 0)
        [ toSuggestion globalKmPackage
        , toSuggestion germanyKmPackage
        , toSuggestion netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (select)"
    requestContext
    "/wizard-api/knowledge-model-packages/suggestions?sort=organizationId,asc&select=org.de:core-de:all,org.nl:core-nl:all"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 2 1 0)
        [ toSuggestion germanyKmPackage
        , toSuggestion netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (exclude)"
    requestContext
    "/wizard-api/knowledge-model-packages/suggestions?sort=organizationId,asc&exclude=org.de:core-de:all,org.nl:core-nl:all"
    (Page "knowledgeModelPackages" (PageMetadata 20 1 1 0) [toSuggestion globalKmPackage])
  create_test_200
    "HTTP 200 OK (query - q)"
    requestContext
    "/wizard-api/knowledge-model-packages/suggestions?q=Germany Knowledge Model"
    (Page "knowledgeModelPackages" (PageMetadata 20 1 1 0) [toSuggestion germanyKmPackage])
  create_test_200
    "HTTP 200 OK (phase)"
    requestContext
    "/wizard-api/knowledge-model-packages/suggestions?sort=organizationId,asc&phase=ReleasedKnowledgeModelPackagePhase"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 3 1 0)
        [ toSuggestion globalKmPackage
        , toSuggestion germanyKmPackage
        , toSuggestion netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    requestContext
    "/wizard-api/knowledge-model-packages/suggestions?q=Non-existing Knowledge Model"
    (Page "knowledgeModelPackages" (PageMetadata 20 0 0 0) ([] :: [KnowledgeModelPackageSuggestion]))

create_test_200 title requestContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO KnowledgeModelPackage.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
