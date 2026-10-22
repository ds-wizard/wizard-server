module Specs.Api.Handler.PersistentCommand.Detail_Attempts_POST (
  detail_attempts_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailJM ()
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.Migration.Development.PersistentCommand.Data.WizardPersistentCommands
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.User.RolePermission
import qualified Shared.Service.Project.Cache.ProjectCacheCommandExecutor as ProjectCacheCommandExecutor
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/persistent-commands/{uuid}/attempts
-- ------------------------------------------------------------------------
detail_attempts_POST :: RequestContext -> SpecWith ((), Application)
detail_attempts_POST requestContext =
  describe "POST /api/persistent-commands/{uuid}/attempts" $ do
    test_201 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = BS.pack $ "/api/persistent-commands/" ++ U.toString command.uuid ++ "/attempts"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO (insertPersistentCommand command) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, PersistentCommandDetailDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ (resDto.state, resDto.attempts) `shouldBe` (ErrorPersistentCommandState, command.attempts + 1)
      -- AND: Compare state in DB with expectation
      eCommandFromDb <- findCommandInDB requestContext
      liftIO $ fmap (\c -> (c.state, c.attempts)) eCommandFromDb `shouldBe` Right (ErrorPersistentCommandState, command.attempts + 1)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext =
  it "HTTP 401 UNAUTHORIZED" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = []
      -- AND: Prepare expectation
      let expStatus = 401
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UnauthorizedError _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO (insertPersistentCommand command) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      eCommandFromDb <- findCommandInDB requestContext
      liftIO $ eCommandFromDb `shouldBe` Right command

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext =
  it "HTTP 403 FORBIDDEN - no required permission" $
    -- GIVEN: Prepare request
    do
      let user = userWithoutPerm requestContext.serverConfig _DEV_USE_ROLE_PERMISSION
      runInContextIO (updateUserByUuid user) requestContext
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__MISSING_PERMISSION _DEV_USE_ROLE_PERMISSION
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO (insertPersistentCommand command) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      eCommandFromDb <- findCommandInDB requestContext
      liftIO $ eCommandFromDb `shouldBe` Right command

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest
    reqMethod
    "/api/persistent-commands/dd4a5b1c-6a0e-4c4f-9a43-2b8e7f3c1d55/attempts"
    reqHeaders
    reqBody
    "persistent_command"
    [("uuid", "dd4a5b1c-6a0e-4c4f-9a43-2b8e7f3c1d55")]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
command :: PersistentCommand U.UUID
command = command1 {state = IgnorePersistentCommandState, component = ProjectCacheCommandExecutor.cComponent, function = ProjectCacheCommandExecutor.cRefreshName}

findCommandInDB = runInContextIO (findPersistentCommandByUuid command.uuid :: RequestContextM (PersistentCommand U.UUID))
