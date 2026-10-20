module Specs.Service.Mcp.Tool.SubmissionPropTools.SetSubmissionProps (
  set_submission_props,
) where

import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.User.UserSubmissionPropDAO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Settings.SettingsMigration
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Public
import Shared.Model.Common.SensitiveData
import Shared.Model.Settings.Settings
import Shared.Model.User.RolePermission
import Shared.Model.User.UserSubmissionProp
import Shared.Model.User.UserSubmissionPropEM ()
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.User.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (set_submission_props)
-- ------------------------------------------------------------------------
set_submission_props :: RequestContext -> SpecWith ((), Application)
set_submission_props requestContext =
  describe "POST /api/mcp (set_submission_props)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqValues = M.fromList [(settingsSubmissionServiceApiTokenProp, "********" :: String), (settingsSubmissionServiceSecretProp, "new secret")]

reqArguments = object ["props" .= M.fromList [(settingsSubmissionService.sId, reqValues)]]

reqBody = toolCallBody "set_submission_props" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expValues = M.fromList [(settingsSubmissionServiceApiTokenProp, "********" :: String), (settingsSubmissionServiceSecretProp, "********")]
      let expContent = object ["services" .= [object ["id" .= settingsSubmissionService.sId, "name" .= settingsSubmissionService.name, "values" .= expValues]]]
      let expDto = toolResult expContent
      let expProps = userAlbertApiToken {values = M.fromList [(settingsSubmissionServiceApiTokenProp, "Some Token"), (settingsSubmissionServiceSecretProp, "new secret")]} :: UserSubmissionProp
      -- AND: Run migrations
      runInContextIO seedSettingsSubmissionService requestContext
      runInContextIO (traverse_ insertOrUpdateUserSubmissionProp userAlbertSubmissionProps) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "set_submission_props" expContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserSubmissionPropsInDB requestContext userAlbert [process defaultSecret expProps]
  it "HTTP 200 OK if the user has no mcp.sensitive (tool error, nothing stored)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION _MCP_SENSITIVE_ROLE_PERMISSION)
      -- AND: Run migrations
      runInContextIO seedSettingsSubmissionService requestContext
      runInContextIO (traverse_ insertOrUpdateUserSubmissionProp userAlbertSubmissionProps) requestContext
      removeSensitivePermission requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserSubmissionPropsInDB requestContext userAlbert userAlbertSubmissionProps

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
