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
import Shared.Constant.Workspace
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Error.Error
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Model.Workspace.Workspace
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/projects
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/projects" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/projects"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (Admin - pagination)"
    requestContext
    "/api/projects?sort=uuid,asc&page=1&size=1"
    reqAuthHeader
    (Page "projects" (PageMetadata 1 6 6 1) [project14Dto])
  create_test_200
    "HTTP 200 OK (Admin - query)"
    requestContext
    "/api/projects?sort=uuid,asc&q=pri"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 2 1 0) [project1Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids)"
    requestContext
    (BS.pack $ "/api/projects?sort=uuid,asc&userUuids=" ++ U.toString userAlbert.uuid)
    reqAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project1Dto, project2Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Admin - userUuids, or)"
    requestContext
    ( BS.pack $
        "/api/projects?sort=uuid,asc&userUuidsOp=or&userUuids="
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
        "/api/projects?sort=uuid,asc&userUuidsOp=and&userUuids="
          ++ U.toString userAlbert.uuid
          ++ ","
          ++ U.toString userIsaac.uuid
    )
    reqAuthHeader
    (Page "projects" (PageMetadata 20 0 0 0) ([] :: [ProjectDTO]))
  create_test_200
    "HTTP 200 OK (Admin - userGroupUuids)"
    requestContext
    (BS.pack $ "/api/projects?sort=uuid,asc&userGroupUuids=" ++ U.toString bioGroup.uuid)
    reqAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project15Dto])
  create_test_200
    "HTTP 200 OK (Admin - userGroupUuids, or)"
    requestContext
    ( BS.pack $
        "/api/projects?sort=uuid,asc&userGroupUuidsOp=or&userGroupUuids="
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
        "/api/projects?sort=uuid,asc&userGroupUuidsOp=and&userGroupUuids="
          ++ U.toString bioGroup.uuid
          ++ ","
          ++ U.toString plantGroup.uuid
    )
    reqAuthHeader
    (Page "projects" (PageMetadata 20 0 0 0) ([] :: [ProjectDTO]))
  create_test_200
    "HTTP 200 OK (Admin - isTemplate - true)"
    requestContext
    "/api/projects?sort=uuid,asc&isTemplate=true"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project14Dto, project1Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Admin - isTemplate - false)"
    requestContext
    "/api/projects?sort=uuid,asc&isTemplate=false"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project3Dto, project15Dto, project2Dto])
  create_test_200
    "HTTP 200 OK (Admin - projectTags)"
    requestContext
    "/api/projects?sort=uuid,asc&projectTags=projectTag1"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 4 1 0)
        [project14Dto, project1Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - projectTags, or)"
    requestContext
    "/api/projects?sort=uuid,asc&projectTagsOp=or&projectTags=projectTag1,projectTag2"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 4 1 0)
        [project14Dto, project1Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - projectTags, and)"
    requestContext
    "/api/projects?sort=uuid,asc&projectTagsOp=and&projectTags=projectTag1,projectTag2"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project2Dto])
  create_test_200
    "HTTP 200 OK (Admin - knowledgePackage)"
    requestContext
    "/api/projects?sort=uuid,asc&knowledgeModelPackageIds=org.nl.amsterdam.core-amsterdam:all"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project14Dto])
  create_test_200
    "HTTP 200 OK (Admin - sort asc)"
    requestContext
    "/api/projects?sort=uuid,asc"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 6 1 0)
        [project3Dto, project14Dto, project1Dto, project15Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Admin - sort desc)"
    requestContext
    "/api/projects?sort=updatedAt,desc"
    reqAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 6 1 0)
        [project15Dto, project3Dto, project14Dto, project1Dto, project12Dto, project2Dto]
    )
  create_test_200
    "HTTP 200 OK (Non-Admin)"
    requestContext
    "/api/projects?sort=uuid,asc"
    reqNonAdminAuthHeader
    ( Page
        "projects"
        (PageMetadata 20 5 1 0)
        [project3Dto, project14Dto, project15Dto, project2Dto, project12Dto]
    )
  create_test_200
    "HTTP 200 OK (Non-Admin - query)"
    requestContext
    "/api/projects?q=pri"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - query users)"
    requestContext
    (BS.pack $ "/api/projects?sort=uuid,asc&userUuids=" ++ U.toString userAlbert.uuid)
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 2 1 0) [project2Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - query user groups)"
    requestContext
    (BS.pack $ "/api/projects?sort=uuid,asc&userGroupUuids=" ++ U.toString bioGroup.uuid)
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project15Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - projectTags)"
    requestContext
    "/api/projects?sort=uuid,asc&projectTags=projectTag1"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project14Dto, project2Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - knowledgeModelPackage)"
    requestContext
    "/api/projects?sort=uuid,asc&knowledgeModelPackageIds=org.nl.amsterdam.core-amsterdam:all"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project14Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isTemplate - true)"
    requestContext
    "/api/projects?sort=uuid,asc&isTemplate=true"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 2 1 0) [project14Dto, project12Dto])
  create_test_200
    "HTTP 200 OK (Non-Admin - isTemplate - false)"
    requestContext
    "/api/projects?sort=uuid,asc&isTemplate=false"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 3 1 0) [project3Dto, project15Dto, project2Dto])
  create_test_200_workspace
    "HTTP 200 OK (Admin - w)"
    requestContext
    insertSecondWorkspace
    (BS.pack $ "/api/projects?sort=uuid,asc&w=" ++ U.toString secondWorkspace.uuid)
    reqAuthHeader
    (Page "projects" (PageMetadata 20 1 1 0) [project16Dto])
  create_test_200_workspace
    "HTTP 200 OK (Admin - all workspaces of the caller)"
    requestContext
    insertSecondWorkspace
    "/api/projects?sort=uuid,asc"
    reqAuthHeader
    (Page "projects" (PageMetadata 20 7 1 0) [project3Dto, project16Dto, project14Dto, project1Dto, project15Dto, project2Dto, project12Dto])
  create_test_200_workspace
    "HTTP 200 OK (Admin - workspace of another tenant)"
    requestContext
    insertSecondWorkspace
    (BS.pack $ "/api/projects?sort=uuid,asc&w=" ++ U.toString differentWorkspaceUuid)
    reqAuthHeader
    (Page "projects" (PageMetadata 20 0 0 0) ([] :: [ProjectDTO]))
  create_test_200_workspace
    "HTTP 200 OK (not a member of w)"
    requestContext
    insertSecondWorkspaceWithoutCaller
    (BS.pack $ "/api/projects?sort=uuid,asc&w=" ++ U.toString secondWorkspaceUuid)
    reqAuthHeader
    (Page "projects" (PageMetadata 20 0 0 0) ([] :: [ProjectDTO]))
  create_test_200_workspace
    "HTTP 200 OK (Non-Admin - only the workspaces the caller is a member of)"
    requestContext
    insertSecondWorkspace
    "/api/projects?sort=uuid,asc"
    reqNonAdminAuthHeader
    (Page "projects" (PageMetadata 20 5 1 0) [project3Dto, project14Dto, project15Dto, project2Dto, project12Dto])

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
test_400 requestContext =
  it "HTTP 400 BAD REQUEST (tenant=true on a workspace-only entity)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED)
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod "/api/projects?tenant=true" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

create_test_200_workspace title requestContext prepareWorkspace reqUrl reqAuthHeader expDto =
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
      demoteToResearcher requestContext userNikola
      prepareWorkspace requestContext
      runInContextIO (insertProject project12) requestContext
      runInContextIO (insertProject project14) requestContext
      runInContextIO (insertProject project15) requestContext
      runInContextIO (insertProject project16) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
