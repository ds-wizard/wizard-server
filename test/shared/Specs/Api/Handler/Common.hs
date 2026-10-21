module Specs.Api.Handler.Common where

import Data.Aeson (Value (..), encode)
import Data.Aeson.Key (toString)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (isRight)
import qualified Data.List as L
import Data.Maybe (fromMaybe, isJust)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import Servant (serve)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Middleware.WizardLoggingMiddleware
import Shared.Api.Resource.Document.DocumentPreviewAcceptedDTO
import Shared.Api.Resource.Document.DocumentPreviewAcceptedJM ()
import Shared.Bootstrap.Web
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.UserToken.Public
import Shared.Model.Cache.ServerCache
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.User.UserToken
import WizardServer.Api.Web
import WizardServer.Model.Context.RequestContext
import WizardServer.Model.Context.ServerContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mail
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
      let expDto = ForbiddenError $ _ERROR_VALIDATION__MISSING_PERMISSION missingPerm
      let expBody = encode expDto
      let expHeaders = resCtHeader : resCorsHeaders
      let expStatus = 403
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

createMcpTokenTest requestContext reqMethod reqUrl otherHeaders reqBody =
  it "HTTP 401 UNAUTHORIZED if the token is an MCP token" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = ("Authorization", BS.pack $ "Bearer " ++ albertMcpToken.value) : otherHeaders
      -- AND: Prepare expectation
      let expStatus = 401
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UnauthorizedError _ERROR_SERVICE_TOKEN__UNABLE_TO_DECODE_AND_VERIFY_TOKEN
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO (insertUserToken albertMcpToken) requestContext
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

assertMailsInDB requestContext expMails = do
  eCommands <- runInContextIO (findPersistentCommands :: RequestContextM [PersistentCommand U.UUID]) requestContext
  liftIO $ isRight eCommands `shouldBe` True
  let (Right commands) = eCommands
  liftIO $ commands `shouldMatchMails` expMails

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

assertEtagHeaders :: ResponseHeaders -> String -> WaiSession () ()
assertEtagHeaders headers hash = liftIO $ do
  lookup "ETag" headers `shouldBe` Just (BS.pack $ "\"" ++ hash ++ "\"")
  lookup "Cache-Control" headers `shouldBe` Just "private, no-cache"
  isJust (lookup "x-trace-uuid" headers) `shouldBe` True

assertAccepted :: SResponse -> Int -> ResponseHeaders -> WaiSession () U.UUID
assertAccepted response expStatus expHeaders = do
  let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, DocumentPreviewAcceptedDTO)
  assertResStatus status expStatus
  assertResHeaders headers expHeaders
  return resDto.documentUuid

contextKeys :: [String]
contextKeys = L.sort ["metamodelVersion", "hash", "workspace", "project", "knowledgeModelEditor", "questionnaire", "knowledgeModel", "knowledgeModelPackage", "document", "config"]

jsonField :: String -> Value -> Value
jsonField key (Object o) = fromMaybe Null (KM.lookup (fromString key) o)
jsonField _ _ = Null

jsonString :: Value -> String
jsonString (String s) = T.unpack s
jsonString _ = ""

jsonKeys :: Value -> [String]
jsonKeys (Object o) = L.sort . fmap toString . KM.keys $ o
jsonKeys _ = []

jsonKeysDeep :: Value -> [String]
jsonKeysDeep (Object o) = fmap toString (KM.keys o) ++ concatMap jsonKeysDeep (KM.elems o)
jsonKeysDeep (Array a) = concatMap jsonKeysDeep a
jsonKeysDeep _ = []
