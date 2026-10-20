module Specs.Api.Handler.Project.Detail_Revert_POST (
  detail_revert_POST,
) where

import Data.Aeson (encode)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectVersions
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Version.ProjectVersion
import Shared.Service.Project.Version.ProjectVersionMapper (toVersionRevertDTO)
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Common
import Specs.Api.Handler.Project.Version.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/projects/{projectUuid}/revert
-- ------------------------------------------------------------------------
detail_revert_POST :: RequestContext -> SpecWith ((), Application)
detail_revert_POST requestContext =
  describe "POST /api/projects/{projectUuid}/revert" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/projects/af984a75-56e3-49f8-b16f-d6b99599910a/revert"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = projectVersion1RevertDto project1Uuid

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = project1CtnRevertedDto
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
      assertExistenceOfProjectInDB requestContext project1 [sre_rQ1' project1Uuid, sre_rQ2' project1Uuid]
      assertAbsenceOfProjectVersionInDB requestContext (projectVersion1 project1Uuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod reqUrl "name"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  it "HTTP 404 NOT FOUND (event of another project)" $
    -- GIVEN: Prepare request
    do
      let foreignEventUuid = (sre_rQ2 project2Uuid).uuid
      let reqBody = encode (toVersionRevertDTO foreignEventUuid)
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND "project_event" [("project_uuid", U.toString project1Uuid), ("uuid", U.toString foreignEventUuid)])
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
      assertExistenceOfProjectInDB requestContext project1 (fEvents project1Uuid)
