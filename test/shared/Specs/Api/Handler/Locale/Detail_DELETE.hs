module Specs.Api.Handler.Locale.Detail_DELETE (
  detail_DELETE,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Locale.Locale
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/locales/{uuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/locales/{uuid}" $ do
    test_204 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = BS.pack $ "/api/locales/" ++ U.toString localeDe.uuid

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT (users of the locale fall back, S3 cleanup is queued)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO (updateUserByUuid userAlbertEditedLocale) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findLocales requestContext 1
      user <- getOneFromDB (findUserByUuid userAlbert.uuid) requestContext
      liftIO $ user.locale `shouldBe` Nothing
      commands <- getOneFromDB (findPersistentCommands :: RequestContextM [PersistentCommand U.UUID]) requestContext
      liftIO $ fmap (\c -> (c.component, c.function)) commands `shouldBe` [("locale", "deleteFromS3")]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    (BS.pack $ "/api/locales/" ++ U.toString differentLocale.uuid)
    reqHeaders
    reqBody
    "locale"
    [("uuid", U.toString differentLocale.uuid)]
