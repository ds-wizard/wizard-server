module Specs.Api.Handler.Project.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.ProjectDTO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/projects
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /wizard-api/projects" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/projects"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (Admin - pagination)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&page=1&size=1"
    reqAuthHeader
    (Page "projects" (PageMetadata 1 6 6 1) [project14Dto])
  create_test_200
    "HTTP 200 OK (Admin - query)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&q=pri"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 2 1 0) [project1Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids)"
    requestContext
    (BS.pack $ "/wizard-api/projects?sort=uuid,asc&userUuids=" ++ U.toString userAlbert.uuid)
    reqAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project1Dto, project2Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids, or)"
    requestContext
    ( BS.pack $
        "/wizard-api/projects?sort=uuid,asc&userUuidsOp=or&userUuids="
          ++ U.toString userAlbert.uuid
          ++ ","
          ++ U.toString userIsaac.uuid
    )
    reqAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project1Dto, project2Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids, and)"
    requestContext
    ( BS.pack $
        "/wizard-api/projects?sort=uuid,asc&userUuidsOp=and&userUuids="
          ++ U.toString userAlbert.uuid
          ++ ","
          ++ U.toString userIsaac.uuid
    )
    reqAuthHeader
    (Page "projects" (PageMetadata 20 0 0 0) ([] :: [ProjectDTO]))
  create_test_200
    "HTTP 200 OK (Admin - userGroupUuids)"
    requestContext
    (BS.pack $ "/wizard-api/projects?sort=uuid,asc&userGroupUuids=" ++ U.toString bioGroup.uuid)
    reqAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project15Dto])
  create_test_200
    "HTTP 200 OK (Admin - userGroupUuids, or)"
    requestContext
    ( BS.pack $
        "/wizard-api/projects?sort=uuid,asc&userGroupUuidsOp=or&userGroupUuids="
          ++ U.toString bioGroup.uuid
          ++ ","
          ++ U.toString plantGroup.uuid
    )
    reqAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project15Dto])
  create_test_200
    "HTTP 200 OK (Admin - userGroupUuids, and)"
    requestContext
    ( BS.pack $
        "/wizard-api/projects?sort=uuid,asc&userGroupUuidsOp=and&userGroupUuids="
          ++ U.toString bioGroup.uuid
          ++ ","
          ++ U.toString plantGroup.uuid
    )
    reqAuthHeader
    (Page "projects" (PageMetadata 20 0 0 0) ([] :: [ProjectDTO]))
  create_test_200
    "HTTP 200 OK (Admin - isTemplate - true)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&isTemplate=true"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project14Dto, project1Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Admin - isTemplate - false)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&isTemplate=false"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project3Dto, project15Dto, project2Dto])
  create_test_200
    "HTTP 200 OK (Admin - projectTags)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&projectTags=projectTag1"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 4 1 0)
        [project14Dto, project1Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - projectTags, or)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&projectTagsOp=or&projectTags=projectTag1,projectTag2"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 4 1 0)
        [project14Dto, project1Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - projectTags, and)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&projectTagsOp=and&projectTags=projectTag1,projectTag2"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project2Dto])
  create_test_200
    "HTTP 200 OK (Admin - knowledgePackage)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&knowledgeModelPackageIds=org.nl.amsterdam:core-amsterdam:all"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project14Dto])
  create_test_200
    "HTTP 200 OK (Admin - sort asc)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 6 1 0)
        [project3Dto, project14Dto, project1Dto, project15Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - sort desc)"
    requestContext
    "/wizard-api/projects?sort=updatedAt,desc"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 6 1 0)
        [project15Dto, project3Dto, project14Dto, project1Dto, project12Dto, project2Dto]
    )
  create_test_200
    "HTTP 200 OK (Non-Admin)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc"
    reqNonAdminAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 5 1 0)
        [project3Dto, project14Dto, project15Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Non-Admin - query)"
    requestContext
    "/wizard-api/projects?q=pri"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - query users)"
    requestContext
    (BS.pack $ "/wizard-api/projects?sort=uuid,asc&userUuids=" ++ U.toString userAlbert.uuid)
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 2 1 0) [project2Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - query user groups)"
    requestContext
    (BS.pack $ "/wizard-api/projects?sort=uuid,asc&userGroupUuids=" ++ U.toString bioGroup.uuid)
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project15Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - projectTags)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&projectTags=projectTag1"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project14Dto, project2Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - knowledgeModelPackage)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&knowledgeModelPackageIds=org.nl.amsterdam:core-amsterdam:all"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project14Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isTemplate - true)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&isTemplate=true"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 2 1 0) [project14Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isTemplate - false)"
    requestContext
    "/wizard-api/projects?sort=uuid,asc&isTemplate=false"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project3Dto, project15Dto, project2Dto])

create_test_200 title requestContext reqUrl reqAuthHeader expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (insertPackage amsterdamKmPackage) requestContext
      runInContextIO (insertProject project12) requestContext
      runInContextIO (insertProject project14) requestContext
      runInContextIO (insertProject project15) requestContext
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
