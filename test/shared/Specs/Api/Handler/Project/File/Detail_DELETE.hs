module Specs.Api.Handler.Project.File.Detail_DELETE (
  detail_DELETE,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Tenant
import Shared.Database.DAO.Project.ProjectFileDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.Project
import Shared.Model.User.User
import Shared.Util.Date
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/projects/{projectUuid}/files/{fileUuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/projects/{projectUuid}/files/{fileUuid}" $ do
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrlT projectUuid fileUuid = BS.pack $ "/api/projects/" ++ U.toString projectUuid ++ "/files/" ++ U.toString fileUuid

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  it "HTTP 404 NOT FOUND (file of another project)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project1.uuid project2File.uuid
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            NotExistsError
              ( _ERROR_DATABASE__ENTITY_NOT_FOUND
                  "project_file"
                  [ ("tenant_uuid", U.toString defaultTenantUuid)
                  , ("project_uuid", U.toString project1.uuid)
                  , ("uuid", U.toString project2File.uuid)
                  ]
              )
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (insertProjectFile project2File) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      eFile <- runInContextIO (findProjectFileByUuid project2File.uuid) requestContext
      liftIO $ isRight eFile `shouldBe` True

project2File :: ProjectFile
project2File =
  ProjectFile
    { uuid = u' "4c3b2a19-0f8e-4d7c-b6a5-948372615f0e"
    , fileName = "project2_file.txt"
    , contentType = "text/plain"
    , fileSize = 123
    , projectUuid = project2.uuid
    , createdBy = Just userAlbert.uuid
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    }
