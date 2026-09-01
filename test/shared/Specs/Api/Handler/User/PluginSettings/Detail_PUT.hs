module Specs.Api.Handler.User.PluginSettings.Detail_PUT (
  detail_PUT,
) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Plugin.Plugin
import Shared.Model.User.UserPluginSettings
import WizardServer.Database.DAO.User.UserPluginSettingsDAO
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Config.Common
import Specs.Api.Handler.User.PluginSettings.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/users/current/plugin-settings/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /wizard-api/users/current/plugin-settings/{uuid}" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = BS.pack $ "/wizard-api/users/current/plugin-settings/" ++ U.toString plugin1.uuid

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = userAlbertPluginSettingsEdited.values

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
      let expDto = userAlbertPluginSettingsEdited.values
      -- AND: Run migrations
      runInContextIO (insertUserPluginSettings userAlbertPluginSettings) requestContext
      runInContextIO (insertUserPluginSettings userCharlesPluginSettings) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, A.Value)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfUserPluginSettingsInDB requestContext userAlbertPluginSettingsEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
