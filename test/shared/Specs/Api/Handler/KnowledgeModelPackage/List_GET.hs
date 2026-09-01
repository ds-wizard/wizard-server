module Specs.Api.Handler.KnowledgeModelPackage.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Shared.Database.Migration.Development.Registry.Data.RegistryPackages
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-packages
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /wizard-api/knowledge-model-packages" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/knowledge-model-packages"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  let expOrgRs = [globalRegistryOrganization, nlRegistryOrganization]
  create_test_200
    "HTTP 200 OK"
    requestContext
    "/wizard-api/knowledge-model-packages?sort=name,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 3 1 0)
        [ toSimpleDTO' [] expOrgRs germanyKmPackage
        , toSimpleDTO' [globalRegistryPackage] expOrgRs globalKmPackage
        , toSimpleDTO' [nlRegistryPackage] expOrgRs netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (query - q)"
    requestContext
    "/wizard-api/knowledge-model-packages?q=Germany Knowledge Model"
    (Page "knowledgeModelPackages" (PageMetadata 20 1 1 0) [toSimpleDTO' [] expOrgRs germanyKmPackage])
  create_test_200
    "HTTP 200 OK (query - kmId)"
    requestContext
    "/wizard-api/knowledge-model-packages?kmId=core-nl"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 1 1 0)
        [toSimpleDTO' [nlRegistryPackage] expOrgRs netherlandsKmPackageV2]
    )
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    requestContext
    "/wizard-api/knowledge-model-packages?q=Non-existing Knowledge Model"
    (Page "knowledgeModelPackages" (PageMetadata 20 0 0 0) ([] :: [KnowledgeModelPackageSimpleDTO]))
  create_test_200
    "HTTP 200 OK (outdated=false)"
    requestContext
    "/wizard-api/knowledge-model-packages?outdated=false&sort=name,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 3 1 0)
        [ toSimpleDTO' [] expOrgRs germanyKmPackage
        , toSimpleDTO' [globalRegistryPackage] expOrgRs globalKmPackage
        , toSimpleDTO' [nlRegistryPackage] expOrgRs netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (outdated=true)"
    requestContext
    "/wizard-api/knowledge-model-packages?outdated=true"
    (Page "knowledgeModelPackages" (PageMetadata 20 0 0 0) ([] :: [KnowledgeModelPackageSimpleDTO]))

create_test_200 title requestContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO R_Migration.runMigration requestContext
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
