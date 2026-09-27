module Specs.Api.Handler.Tenant.Config.List_PUT (
  list_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Tenant.Config.WizardTenantConfigChangeDTO hiding (request)
import Shared.Api.Resource.Tenant.Config.WizardTenantConfigJM ()
import Shared.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.Tenant.Config.WizardTenantConfig hiding (request)
import Shared.Model.User.Role
import WizardServer.Api.Resource.Tenant.Config.TenantConfigChangeJM ()
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Config.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/tenants/current/config
-- ------------------------------------------------------------------------
list_PUT :: RequestContext -> SpecWith ((), Application)
list_PUT requestContext =
  describe "PUT /api/tenants/current/config" $ do
    test_200 requestContext
    test_400_invalid_json requestContext
    test_400_workspace_role requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/api/tenants/current/config"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = defaultTenantConfigChangeDto {project = editedProjectChangeDto} :: TenantConfigChangeDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = defaultTenantConfig {project = editedProject} :: TenantConfig
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO (insertOrUpdateConfigSubmissionService defaultSubmissionService) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, TenantConfig)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfTenantConfigProjectInDB requestContext editedProject

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json requestContext = createInvalidJsonTest reqMethod reqUrl "uuid"

test_400_workspace_role requestContext =
  it "HTTP 400 BAD REQUEST if the default role is a workspace role" $
    -- GIVEN: Prepare request
    do
      let authenticationDto = defaultAuthenticationChangeDto {defaultRoleUuid = defaultWorkspaceAdminRole.uuid} :: TenantConfigAuthenticationChangeDTO
      let reqDto = defaultTenantConfigChangeDto {authentication = authenticationDto} :: TenantConfigChangeDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (UserError _ERROR_VALIDATION__USER_ROLE_NOT_ORGANIZATION)
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO (insertOrUpdateConfigSubmissionService defaultSubmissionService) requestContext
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

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "organizationSettings.manage"
