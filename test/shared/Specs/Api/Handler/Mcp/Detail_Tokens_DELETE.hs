module Specs.Api.Handler.Mcp.Detail_Tokens_DELETE (
  detail_tokens_DELETE,
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
import Shared.Constant.Tenant
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.Migration.Development.Mcp.Data.McpRefreshTokens
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Public
import Shared.Model.Cache.ServerCache
import Shared.Model.Error.Error
import Shared.Model.User.User
import Shared.Model.User.UserToken
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/mcp/tokens/{uuid}
-- ------------------------------------------------------------------------
detail_tokens_DELETE :: RequestContext -> SpecWith ((), Application)
detail_tokens_DELETE requestContext =
  describe "DELETE /api/mcp/tokens/{uuid}" $ do
    test_204 requestContext
    test_401 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = tokenUrl mcpGrantToken

tokenUrl :: UserToken -> BS.ByteString
tokenUrl userToken = BS.pack $ "/api/mcp/tokens/" ++ U.toString userToken.uuid

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO (insertUserToken mcpGrantToken) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfUserTokenInDB requestContext mcpGrantToken

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext = do
  create_test_404 "HTTP 404 NOT FOUND if the token belongs to another user" requestContext charlesMcpToken
  create_test_404 "HTTP 404 NOT FOUND if the token is not an MCP token" requestContext albertLoginToken

create_test_404 title requestContext userToken =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = tokenUrl userToken
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "user_token" [("tenant_uuid", U.toString defaultTenantUuid), ("uuid", U.toString userToken.uuid)]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO (insertUserToken userToken) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserTokenInDB requestContext userToken

charlesMcpToken :: UserToken
charlesMcpToken =
  mcpGrantToken
    { uuid = u' "f1d9b7a5-3c4e-4f8a-8b2d-5e7a9c1f3b68"
    , userUuid = userCharles.uuid
    , value = "charles-mcp-access-token"
    }

albertLoginToken :: UserToken
albertLoginToken =
  mcpGrantToken
    { uuid = u' "a2e0c8b6-4d5f-4a9b-9c3e-6f8b0d2a4c79"
    , tType = LoginUserTokenType
    , value = "albert-second-login-token"
    }
