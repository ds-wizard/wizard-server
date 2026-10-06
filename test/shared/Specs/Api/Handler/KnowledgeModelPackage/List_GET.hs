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
import Shared.Database.Migration.Development.Registry.Data.RegistryPackages
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Workspace.Workspace
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import Shared.Database.DAO.Package.KnowledgeModelPackageDAO

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/knowledge-model-packages
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/knowledge-model-packages" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/knowledge-model-packages"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    requestContext
    "/api/knowledge-model-packages?sort=name,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 3 1 0)
        [ toSimpleDTO' [] germanyKmPackage
        , toSimpleDTO' [globalRegistryPackage] globalKmPackage
        , toSimpleDTO' [nlRegistryPackage] netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (query - q)"
    requestContext
    "/api/knowledge-model-packages?q=Germany Knowledge Model"
    (Page "knowledgeModelPackages" (PageMetadata 20 1 1 0) [toSimpleDTO' [] germanyKmPackage])
  create_test_200
    "HTTP 200 OK (query - id)"
    requestContext
    "/api/knowledge-model-packages?id=org.nl.core-nl"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 1 1 0)
        [toSimpleDTO' [nlRegistryPackage] netherlandsKmPackageV2]
    )
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    requestContext
    "/api/knowledge-model-packages?q=Non-existing Knowledge Model"
    (Page "knowledgeModelPackages" (PageMetadata 20 0 0 0) ([] :: [KnowledgeModelPackageSimpleDTO]))
  create_test_200
    "HTTP 200 OK (outdated=false)"
    requestContext
    "/api/knowledge-model-packages?outdated=false&sort=name,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 3 1 0)
        [ toSimpleDTO' [] germanyKmPackage
        , toSimpleDTO' [globalRegistryPackage] globalKmPackage
        , toSimpleDTO' [nlRegistryPackage] netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (outdated=true)"
    requestContext
    "/api/knowledge-model-packages?outdated=true"
    (Page "knowledgeModelPackages" (PageMetadata 20 0 0 0) ([] :: [KnowledgeModelPackageSimpleDTO]))
  create_test_200_workspace
    "HTTP 200 OK (tenant=true - tenant rows only)"
    requestContext
    insertSecondWorkspace
    "/api/knowledge-model-packages?tenant=true&sort=name,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 3 1 0)
        [ toSimpleDTO' [] germanyKmPackage
        , toSimpleDTO' [globalRegistryPackage] globalKmPackage
        , toSimpleDTO' [nlRegistryPackage] netherlandsKmPackageV2
        ]
    )
  create_test_200_workspace
    "HTTP 200 OK (w - tenant and workspace rows)"
    requestContext
    insertSecondWorkspace
    "/api/knowledge-model-packages?w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9&sort=name,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 4 1 0)
        [ toSimpleDTO' [] germanyKmPackage
        , toSimpleDTO' [globalRegistryPackage] globalKmPackage
        , toSimpleDTO' [nlRegistryPackage] netherlandsKmPackageV2
        , toSimpleDTO' [] workspaceKmPackage
        ]
    )
  create_test_200_workspace
    "HTTP 200 OK (no parameter - every plane of the caller)"
    requestContext
    insertSecondWorkspace
    "/api/knowledge-model-packages?sort=name,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 4 1 0)
        [ toSimpleDTO' [] germanyKmPackage
        , toSimpleDTO' [globalRegistryPackage] globalKmPackage
        , toSimpleDTO' [nlRegistryPackage] netherlandsKmPackageV2
        , toSimpleDTO' [] workspaceKmPackage
        ]
    )
  create_test_200_workspace
    "HTTP 200 OK (not a member of w - empty page)"
    requestContext
    insertSecondWorkspaceWithoutCaller
    "/api/knowledge-model-packages?w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9&sort=name,asc"
    (Page "knowledgeModelPackages" (PageMetadata 20 0 0 0) ([] :: [KnowledgeModelPackageSimpleDTO]))
  create_test_200_workspace
    "HTTP 200 OK (w - same coordinate in the tenant and the workspace plane)"
    requestContext
    insertSecondWorkspaceWithNetherlandsKmPackage
    "/api/knowledge-model-packages?id=org.nl.core-nl&w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9&sort=name,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 2 1 0)
        [ toSimpleDTO' [nlRegistryPackage] netherlandsKmPackageV2
        , toSimpleDTO' [nlRegistryPackage] workspaceNetherlandsKmPackage
        ]
    )
  create_test_200_workspace
    "HTTP 200 OK (tenant=true - same coordinate only once)"
    requestContext
    insertSecondWorkspaceWithNetherlandsKmPackage
    "/api/knowledge-model-packages?id=org.nl.core-nl&tenant=true&sort=name,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 1 1 0)
        [toSimpleDTO' [nlRegistryPackage] netherlandsKmPackageV2]
    )

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

create_test_200_workspace title requestContext prepareWorkspace reqUrl expDto =
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
      prepareWorkspace requestContext
      runInContextIO (insertPackage workspaceKmPackage) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

workspaceKmPackage :: KnowledgeModelPackage
workspaceKmPackage =
  germanyKmPackage
    { uuid = u' "9c1d2e3f-4a5b-4c6d-8e7f-0a1b2c3d4e5f"
    , name = "Workspace Knowledge Model"
    , id = "org.de.core-ws"
    , workspaceUuid = Just secondWorkspace.uuid
    }

insertSecondWorkspaceWithNetherlandsKmPackage requestContext = do
  insertSecondWorkspace requestContext
  runInContextIO (insertPackage workspaceNetherlandsKmPackage) requestContext

workspaceNetherlandsKmPackage :: KnowledgeModelPackage
workspaceNetherlandsKmPackage =
  netherlandsKmPackage
    { uuid = u' "5d6e7f80-9a1b-4c2d-8e3f-4a5b6c7d8e9f"
    , name = "Netherlands Workspace Knowledge Model"
    , workspaceUuid = Just secondWorkspace.uuid
    }

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
