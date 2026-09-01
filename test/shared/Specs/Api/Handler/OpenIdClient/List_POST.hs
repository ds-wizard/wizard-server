module Specs.Api.Handler.OpenIdClient.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM ()
import Shared.Database.DAO.OpenId.OpenIdClientDefinitionDAO (findOpenIdClientDefinitions)
import Shared.Database.Migration.Development.OpenId.Data.OpenIdClients
import qualified Shared.Database.Migration.Development.OpenId.OpenIdClientMigration as OPENID_Migration
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.OpenIdClient.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/open-id-clients
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /wizard-api/open-id-clients" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/open-id-clients"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto :: OpenIdClientChangeDTO
reqDto = defaultOpenIdClientChangeDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = editedOpenIdClientDetailDto
    -- AND: Run migrations
    runInContextIO OPENID_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, OpenIdClientDetailDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareOpenIdClientDetailDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findOpenIdClientDefinitions requestContext 2

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
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "SettingsManageRolePermission"
