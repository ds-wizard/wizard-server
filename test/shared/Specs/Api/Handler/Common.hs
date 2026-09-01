module Specs.Api.Handler.Common where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (isRight)
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Servant (serve)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Middleware.WizardLoggingMiddleware
import Shared.Bootstrap.Web
import Shared.Database.DAO.User.UserDAO
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Public
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Error.Error
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.User.UserToken
import WizardServer.Api.Web
import WizardServer.Model.Context.RequestContext
import WizardServer.Model.Context.ServerContext

import SharedTest.Specs.Api.Common
import Specs.Common

startWebApp :: ServerContext -> RequestContext -> IO Application
startWebApp serverContext requestContext = do
  let config = requestContext.serverConfig
  let webPort = config.general.serverPort
  let env = config.general.environment
  return $ runMiddleware env loggingMiddleware $ serve webApi (webServer serverContext)

reqAuthToken :: String
reqAuthToken = albertToken.value

reqAuthHeader :: Header
reqAuthHeader = ("Authorization", BS.pack $ "Bearer " ++ reqAuthToken)

reqNonAdminAuthToken :: String
reqNonAdminAuthToken = nikolaToken.value

reqNonAdminAuthHeader :: Header
reqNonAdminAuthHeader = ("Authorization", BS.pack $ "Bearer " ++ reqNonAdminAuthToken)

reqIsaacAuthToken :: String
reqIsaacAuthToken = isaacToken.value

reqIsaacAuthTokenHeader :: Header
reqIsaacAuthTokenHeader = ("Authorization", BS.pack $ "Bearer " ++ reqIsaacAuthToken)

userWithoutPerm :: ServerConfig -> String -> User
userWithoutPerm _ perm =
  userAlbert {role = userAlbert.role {permissions = filter (/= perm) allRolePermissions}}

userWithoutPerms :: ServerConfig -> [String] -> User
userWithoutPerms _ perms =
  userAlbert {role = userAlbert.role {permissions = filter (`notElem` perms) allRolePermissions}}

createInvalidJsonTest reqMethod reqUrl missingField =
  it "HTTP 400 BAD REQUEST when json is not valid" $ do
    let reqHeaders = [reqAuthHeader, reqCtHeader]
    let reqBody = BSL.pack "{}"
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeaderUtf8 : resCorsHeaders
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyContainsInvalidJsonMessage}
    response `shouldRespondWith` responseMatcher

createNoPermissionTest requestContext reqMethod reqUrl otherHeaders reqBody missingPerm =
  it "HTTP 403 FORBIDDEN - no required permission" $
    -- GIVEN: Prepare request
    do
      let user = userWithoutPerm requestContext.serverConfig missingPerm
      runInContextIO (updateUserByUuid user) requestContext
      let reqHeaders = reqAuthHeader : otherHeaders
      -- GIVEN: Prepare expectation
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ missingPerm)
      let expBody = encode expDto
      let expHeaders = resCtHeader : resCorsHeaders
      let expStatus = 403
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

createNoPermissionsAnyTest requestContext reqMethod reqUrl otherHeaders reqBody missingPerms =
  it "HTTP 403 FORBIDDEN - no required permission" $
    -- GIVEN: Prepare request
    do
      let user = userWithoutPerms requestContext.serverConfig missingPerms
      runInContextIO (updateUserByUuid user) requestContext
      let reqHeaders = reqAuthHeader : otherHeaders
      -- GIVEN: Prepare expectation
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission (need any): " ++ show missingPerms)
      let expBody = encode expDto
      let expHeaders = resCtHeader : resCorsHeaders
      let expStatus = 403
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

assertCountInDB dbFunction requestContext count = do
  eitherList <- runInContextIO dbFunction requestContext
  liftIO $ isRight eitherList `shouldBe` True
  let (Right list) = eitherList
  liftIO $ L.length list `shouldBe` count

getFirstFromDB dbFunction requestContext = do
  eitherList <- runInContextIO dbFunction requestContext
  liftIO $ isRight eitherList `shouldBe` True
  let (Right list) = eitherList
  return . head $ list

getOneFromDB dbFunction requestContext = do
  eitherOne <- runInContextIO dbFunction requestContext
  liftIO $ isRight eitherOne `shouldBe` True
  let (Right one) = eitherOne
  return one
