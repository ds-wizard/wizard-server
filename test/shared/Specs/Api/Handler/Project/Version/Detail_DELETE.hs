module Specs.Api.Handler.Project.Version.Detail_DELETE (
  detail_DELETE,
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
import Shared.Constant.Tenant
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.Project.Data.ProjectVersions
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Version.ProjectVersion
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Version.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/projects/{projectUuid}/versions/{vUuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/projects/{projectUuid}/versions/{vUuid}" $ do
    test_204 requestContext
    test_401 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/api/projects/af984a75-56e3-49f8-b16f-d6b99599910a/versions/af984a75-56e3-49f8-b16f-dd016270ce7e"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertAbsenceOfProjectVersionInDB requestContext (projectVersion1 project1Uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext = do
  createNotFoundTest'
    reqMethod
    "/api/projects/00084a75-56e3-49f8-b16f-d6b99599910a/versions/00084a75-56e3-49f8-b16f-dd016270ce7e"
    reqHeaders
    reqBody
    "project"
    [("uuid", "00084a75-56e3-49f8-b16f-d6b99599910a")]
  it "HTTP 404 NOT FOUND (version of another project)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = BS.pack $ "/api/projects/" ++ U.toString project1Uuid ++ "/versions/" ++ U.toString (projectVersion1 project2Uuid).uuid
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            NotExistsError
              ( _ERROR_DATABASE__ENTITY_NOT_FOUND
                  "project_version"
                  [ ("tenant_uuid", U.toString defaultTenantUuid)
                  , ("project_uuid", U.toString project1Uuid)
                  , ("uuid", U.toString (projectVersion1 project2Uuid).uuid)
                  ]
              )
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfProjectVersionInDB requestContext (projectVersion1 project2Uuid)
