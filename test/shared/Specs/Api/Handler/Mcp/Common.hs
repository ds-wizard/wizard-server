module Specs.Api.Handler.Mcp.Common where

import Control.Monad (unless, void, (<=<))
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (mapMaybe)
import Data.Time
import qualified Data.UUID as U
import Network.HTTP.Types (Header, hContentType, methodPost)
import Network.Wai (Application)
import Network.Wai.Test (SResponse (..))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Mcp.McpAuthorizationCodeDAO
import Shared.Database.DAO.Mcp.McpClientDAO
import Shared.Database.DAO.Mcp.McpRefreshTokenDAO
import Shared.Database.DAO.Project.ProjectPermDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Public
import Shared.Model.Cache.ServerCache
import Shared.Model.Localization.LocaleRecord
import Shared.Model.Mcp.McpAuthorizationCode
import Shared.Model.Mcp.McpClient
import Shared.Model.Mcp.McpRefreshToken
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.User.RolePermission
import Shared.Model.User.RoleSimple
import Shared.Model.User.User
import Shared.Model.User.UserToken
import Shared.Util.Crypto (hashSHA256)
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
reqMcpAuthHeader :: Header
reqMcpAuthHeader = ("Authorization", BS.pack $ "Bearer " ++ albertMcpToken.value)

reqFormHeader :: Header
reqFormHeader = (hContentType, "application/x-www-form-urlencoded")

basicAuthHeader :: Header
basicAuthHeader = ("Authorization", "Basic YXVkaXQtbG9nLWNsaWVudDphdWRpdC1sb2ctc2VjcmV0")

mcpIssuer :: String
mcpIssuer = "http://localhost:3000/api/mcp"

authorizationServerMetadata :: Value
authorizationServerMetadata =
  object
    [ "issuer" .= mcpIssuer
    , "authorization_endpoint" .= ("http://localhost:8080/mcp" :: String)
    , "token_endpoint" .= (mcpIssuer ++ "/oauth2/token")
    , "registration_endpoint" .= (mcpIssuer ++ "/oauth2/register")
    , "jwks_uri" .= (mcpIssuer ++ "/oauth2/keys")
    , "response_types_supported" .= ["code" :: String]
    , "response_modes_supported" .= ["query" :: String]
    , "grant_types_supported" .= ["authorization_code" :: String, "refresh_token"]
    , "token_endpoint_auth_methods_supported" .= ["client_secret_post" :: String, "client_secret_basic", "none"]
    , "code_challenge_methods_supported" .= ["S256" :: String]
    , "scopes_supported" .= ([] :: [String])
    , "subject_types_supported" .= ["public" :: String]
    , "id_token_signing_alg_values_supported" .= ["RS256" :: String]
    ]

protectedResourceMetadata :: Value
protectedResourceMetadata =
  object
    [ "resource" .= mcpIssuer
    , "authorization_servers" .= [mcpIssuer]
    , "bearer_methods_supported" .= ["header" :: String]
    , "scopes_supported" .= ([] :: [String])
    ]

oauthError :: String -> String -> Value
oauthError errorCode errorDescription = object ["error" .= errorCode, "error_description" .= errorDescription]

eventStreamMessages :: SResponse -> [Value]
eventStreamMessages response = mapMaybe (decode <=< BSL.stripPrefix "data: ") (BSL.lines (simpleBody response))

resultSizeOf :: SResponse -> Maybe Int
resultSizeOf response = fromIntegral . BSL.length . encode <$> (valueField "result" =<< decode (simpleBody response))

unknownUuid :: U.UUID
unknownUuid = u' "78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e"

removeSensitivePermission :: RequestContext -> WaiSession st ()
removeSensitivePermission requestContext = do
  let permissions = filter (/= _MCP_SENSITIVE_ROLE_PERMISSION) userAlbert.role.permissions
  void $ runInContextIO (updateUserByUuid (userAlbert {role = userAlbert.role {permissions = permissions} :: RoleSimple} :: User)) requestContext

removeAlbertPermissions :: RequestContext -> [String] -> WaiSession st ()
removeAlbertPermissions requestContext removed = do
  let permissions = filter (`notElem` removed) userAlbert.role.permissions
  void $ runInContextIO (updateUserByUuid (userAlbert {role = userAlbert.role {permissions = permissions} :: RoleSimple} :: User)) requestContext

grantIsaac :: RequestContext -> [String] -> WaiSession st ()
grantIsaac requestContext projectPerms = do
  let role = userIsaac.role {permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]} :: RoleSimple
  void $ runInContextIO (updateUserByUuid (userIsaac {role = role} :: User)) requestContext
  unless (null projectPerms) . void $
    runInContextIO (insertProjectPerm (project1AlbertEditProjectPerm {memberUuid = userIsaac.uuid, perms = projectPerms})) requestContext

createIsaacToolErrorTest :: String -> RequestContext -> WaiSession () a -> [String] -> BSL.ByteString -> LocaleRecord -> SpecWith ((), Application)
createIsaacToolErrorTest title requestContext runMigrations projectPerms reqBody expError =
  it ("HTTP 200 OK if " ++ title ++ " (tool error)") $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage expError)
      -- AND: Run migrations
      void runMigrations
      grantIsaac requestContext projectPerms
      -- WHEN: Call API
      response <- request methodPost "/api/mcp" [reqIsaacAuthTokenHeader, reqCtHeader] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfMcpAuthorizationCodeInDB :: RequestContext -> String -> McpAuthorizationCode -> WaiSession () ()
assertExistenceOfMcpAuthorizationCodeInDB requestContext codeValue expCode = do
  codeFromDb <- getOneFromDB (findMcpAuthorizationCodeByCode (hashSHA256 codeValue)) requestContext
  liftIO $ codeFromDb `shouldBe` expCode {uuid = codeFromDb.uuid, code = codeFromDb.code, expiresAt = codeFromDb.expiresAt, createdAt = codeFromDb.createdAt}

assertAbsenceOfMcpAuthorizationCodeInDB :: RequestContext -> String -> WaiSession () ()
assertAbsenceOfMcpAuthorizationCodeInDB requestContext codeHash = do
  mCodeFromDb <- getOneFromDB (findMcpAuthorizationCodeByCode' codeHash) requestContext
  liftIO $ mCodeFromDb `shouldBe` Nothing

assertExistenceOfMcpRefreshTokenInDB :: RequestContext -> String -> McpRefreshToken -> WaiSession () ()
assertExistenceOfMcpRefreshTokenInDB requestContext tokenValue expToken = do
  Just tokenFromDb <- getOneFromDB (findMcpRefreshTokenByTokenHash' (hashSHA256 tokenValue)) requestContext
  liftIO $ tokenFromDb `shouldBe` expToken {uuid = tokenFromDb.uuid, tokenHash = hashSHA256 tokenValue, expiresAt = tokenFromDb.expiresAt, createdAt = tokenFromDb.createdAt}

assertAbsenceOfMcpRefreshTokenInDB :: RequestContext -> String -> WaiSession () ()
assertAbsenceOfMcpRefreshTokenInDB requestContext tokenHash = do
  mTokenFromDb <- getOneFromDB (findMcpRefreshTokenByTokenHash' tokenHash) requestContext
  liftIO $ mTokenFromDb `shouldBe` Nothing

assertMcpGrantTokenInDB :: RequestContext -> String -> WaiSession () UserToken
assertMcpGrantTokenInDB requestContext accessToken = do
  now <- liftIO getCurrentTime
  userTokens <- getOneFromDB (findUserTokensByUserUuidAndType userAlbert.uuid "McpUserTokenType") requestContext
  liftIO $ fmap (.value) userTokens `shouldBe` [accessToken]
  liftIO $ fmap ((> addUTCTime (89 * nominalDay) now) . (.expiresAt)) userTokens `shouldBe` [True]
  return (head userTokens)

assertAbsenceOfUserTokenInDB :: RequestContext -> UserToken -> WaiSession () ()
assertAbsenceOfUserTokenInDB requestContext userToken = do
  userTokens <- getOneFromDB findUserTokens requestContext
  liftIO $ (userToken.uuid `elem` fmap (.uuid) userTokens) `shouldBe` False

assertExistenceOfUserTokenInDB :: RequestContext -> UserToken -> WaiSession () ()
assertExistenceOfUserTokenInDB requestContext userToken = do
  userTokenFromDb <- getOneFromDB (findUserTokenByUuid userToken.uuid) requestContext
  liftIO $ userTokenFromDb.uuid `shouldBe` userToken.uuid

assertExistenceOfMcpClientInDB :: RequestContext -> String -> McpClient -> WaiSession () ()
assertExistenceOfMcpClientInDB requestContext clientId expClient = do
  clientFromDb <- getOneFromDB (findMcpClientByClientId clientId) requestContext
  liftIO $ clientFromDb `shouldBe` expClient {uuid = clientFromDb.uuid, clientId = clientId, clientSecret = clientFromDb.clientSecret, createdAt = clientFromDb.createdAt, updatedAt = clientFromDb.updatedAt}
  liftIO $ null clientFromDb.clientSecret `shouldBe` (expClient.tokenEndpointAuthMethod == "none")

-- --------------------------------
-- TESTS
-- --------------------------------
createToolNoPermissionTest requestContext reqBody permission =
  it ("HTTP 200 OK if the user has no " ++ permission ++ " (tool error)") $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION permission)
      -- AND: Run migrations
      removeAlbertPermissions requestContext [permission]
      -- WHEN: Call API
      response <- request methodPost "/api/mcp" [reqAuthHeader, reqCtHeader] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
