module Specs.Api.Handler.Tenant.Detail_DELETE (
  detail_DELETE,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import qualified Shared.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Model.Tenant.Tenant
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Tenant.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/tenants/{uuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/tenants/{uuid}" $ do
    test_204 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = BS.pack $ "/api/tenants/" ++ U.toString differentTenant.uuid

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      runInContextIO DOC_Migration.runMigration requestContext
      runInContextIO LOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfTenantInDB requestContext differentTenant
      assertAbsenceOfTenantDataInDB requestContext differentTenant

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody "tenants.manage"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest
    reqMethod
    "/api/tenants/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "tenant"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
