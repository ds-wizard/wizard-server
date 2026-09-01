module Specs.Api.Handler.Tenant.PluginSettings.Detail_PUT (
  detail_PUT,
) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Model.Plugin.Plugin
import WizardServer.Database.DAO.Tenant.PluginSettings.TenantPluginSettingsDAO
import WizardServer.Database.Migration.Development.Tenant.Data.TenantPluginSettings
import WizardServer.Model.Context.RequestContext
import WizardServer.Model.Tenant.PluginSettings.TenantPluginSettings

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Config.Common
import Specs.Api.Handler.Tenant.PluginSettings.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/tenants/current/plugin-settings/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /wizard-api/tenants/current/plugin-settings/{uuid}" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = BS.pack $ "/wizard-api/tenants/current/plugin-settings/" ++ U.toString plugin1.uuid

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = plugin1Values1Edited

reqBody = A.encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = defaultTenantPluginSettingsEdited.values
      -- AND: Run migrations
      runInContextIO (insertTenantPluginSettings defaultTenantPluginSettings) requestContext
      runInContextIO (insertTenantPluginSettings differentTenantPluginSettings) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, A.Value)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfTenantPluginSettingsInDB requestContext defaultTenantPluginSettingsEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
