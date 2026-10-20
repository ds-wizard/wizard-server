module Specs.Service.UserToken.UserTokenServiceSpec where

import Data.Maybe (isJust, isNothing)
import Test.Hspec

import Shared.Database.DAO.Mcp.McpAuthorizationCodeDAO
import Shared.Database.DAO.Mcp.McpRefreshTokenDAO
import Shared.Database.Migration.Development.Mcp.Data.McpAuthorizationCodes
import Shared.Database.Migration.Development.Mcp.Data.McpRefreshTokens
import Shared.Model.Cache.ServerCache
import Shared.Model.Mcp.McpAuthorizationCode
import Shared.Model.Mcp.McpRefreshToken
import Shared.Service.UserToken.UserTokenService
import Shared.Util.Date
import Shared.Util.Uuid

import Specs.Common

userTokenServiceSpec requestContext =
  describe "UserTokenService" $
    describe "cleanTokens" $ do
      it "deletes an expired authorization code and keeps a live one" $
        -- GIVEN:
        do
          runInContextIO (insertMcpAuthorizationCode mcpAuthorizationCode) requestContext
          runInContextIO (insertMcpAuthorizationCode mcpExpiredAuthorizationCode) requestContext
          -- WHEN:
          (Right ()) <- runInContext cleanTokens requestContext
          -- THEN:
          (Right mExpired) <- runInContext (findMcpAuthorizationCodeByCode' mcpExpiredAuthorizationCode.code) requestContext
          (Right mLive) <- runInContext (findMcpAuthorizationCodeByCode' mcpAuthorizationCode.code) requestContext
          isNothing mExpired `shouldBe` True
          isJust mLive `shouldBe` True
      it "deletes an expired refresh token and keeps a live one" $
        -- GIVEN:
        do
          runInContextIO (insertMcpRefreshToken mcpLiveRefreshToken) requestContext
          runInContextIO (insertMcpRefreshToken expiredRefreshToken) requestContext
          -- WHEN:
          (Right ()) <- runInContext cleanTokens requestContext
          -- THEN:
          (Right mExpired) <- runInContext (findMcpRefreshTokenByTokenHash' expiredRefreshToken.tokenHash) requestContext
          (Right mLive) <- runInContext (findMcpRefreshTokenByTokenHash' mcpLiveRefreshToken.tokenHash) requestContext
          isNothing mExpired `shouldBe` True
          isJust mLive `shouldBe` True

expiredRefreshToken :: McpRefreshToken
expiredRefreshToken =
  mcpLiveRefreshToken
    { uuid = u' "6e1f3a5b-7c9d-4e2f-8b4a-1c3e5d7f9b84"
    , tokenHash = "expired-refresh-token-hash"
    , expiresAt = dt' 2020 1 1
    }
