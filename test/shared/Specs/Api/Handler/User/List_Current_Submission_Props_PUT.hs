module Specs.Api.Handler.User.List_Current_Submission_Props_PUT (
  list_current_submission_props_PUT,
) where

import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.User.UserSubmissionPropJM ()
import Shared.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Shared.Database.DAO.User.UserSubmissionPropDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs
import Shared.Database.Migration.Development.User.Data.WizardUsers
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/users/current/submission-props
-- ------------------------------------------------------------------------
list_current_submission_props_PUT :: RequestContext -> SpecWith ((), Application)
list_current_submission_props_PUT requestContext =
  describe "PUT /wizard-api/users/current/submission-props" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/users/current/submission-props"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = [userAlbertApiTokenEditedDto]

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
      let expDto = [userAlbertApiTokenEditedDto]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO (insertOrUpdateConfigSubmissionService defaultSubmissionService) requestContext
      runInContextIO (traverse_ insertOrUpdateUserSubmissionProp userAlbertSubmissionProps) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher = ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfUserSubmissionPropsInDB requestContext userAlbert userAlbertSubmissionPropsEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod reqUrl "password"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
