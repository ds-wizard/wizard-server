module Specs.Api.Handler.Tenant.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Tenant.TenantCreateDTO
import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Api.Resource.Tenant.WizardTenantJM ()
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Service.Tenant.TenantMapper (toClientUrlBase)
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/tenants
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /wizard-api/tenants" $ do
    test_201 requestContext
    test_400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/tenants"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT dto = dto

reqBodyT dto = encode (reqDtoT dto)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext = do
  create_test_201 "HTTP 201 CREATED (anonymous)" requestContext tenantCreateDto [] 1 False
  create_test_201 "HTTP 201 CREATED (admin)" requestContext tenantCreateDto [reqAuthHeader] 0 True

create_test_201 title requestContext reqDto authHeaders persistentCommandCount userActive =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeaders
      let reqBody = reqBodyT reqDto
      -- GIVEN: Prepare expectation
      let expStatus = 201
      let expHeaders = resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, TenantDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      -- AND: Find result in DB and compare with expectation state
      (Right tenant) <- runInContextIO (findTenantByClientUrl (toClientUrlBase resDto.clientUrl)) requestContext
      let updatedRequestContext = requestContext {currentTenantUuid = tenant.uuid}
      (Right [user]) <- runInContextIO findUsers updatedRequestContext
      liftIO $ user.active `shouldBe` userActive
      assertCountInDB (findUserEmailLinks :: RequestContextM [UserEmailLink U.UUID UserEmailLinkType]) updatedRequestContext 1
      assertCountInDB (findPersistentCommands :: RequestContextM [PersistentCommand U.UUID]) updatedRequestContext persistentCommandCount

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "lastName"
  create_test_400_tenant_id_uniqueness "HTTP 400 BAD REQUEST if tenantId is already used (anonymous)" requestContext []
  create_test_400_tenant_id_uniqueness "HTTP 400 BAD REQUEST if tenantId is already used (admin)" requestContext [reqAuthHeader]

create_test_400_tenant_id_uniqueness title requestContext authHeaders =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeaders
      let reqDto = tenantCreateDto {tenantId = "default"} :: TenantCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "tenantId" [_ERROR_VALIDATION__TENANT_ID_UNIQUENESS])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
