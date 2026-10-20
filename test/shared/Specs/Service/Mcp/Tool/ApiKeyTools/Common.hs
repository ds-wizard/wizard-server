module Specs.Service.Mcp.Tool.ApiKeyTools.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Tenant
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.User.User
import Shared.Model.User.UserToken
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertUserTokensOfTypeInDB :: RequestContext -> String -> [UserToken] -> WaiSession () ()
assertUserTokensOfTypeInDB requestContext tokenType expTokens = do
  tokens <- getOneFromDB (findUserTokensByUserUuidAndType userAlbert.uuid tokenType) requestContext
  liftIO $ tokens `shouldBe` expTokens

assertExistenceOfApiKeyInDB :: RequestContext -> String -> String -> WaiSession () ()
assertExistenceOfApiKeyInDB requestContext expName expValue = do
  apiKeys <- getOneFromDB (findUserTokensByUserUuidAndType userAlbert.uuid "ApiKeyUserTokenType") requestContext
  let expApiKey apiKey = apiKey {name = expName, tType = ApiKeyUserTokenType, userUuid = userAlbert.uuid, value = expValue, tenantUuid = defaultTenantUuid} :: UserToken
  liftIO $ apiKeys `shouldBe` fmap expApiKey apiKeys
  liftIO $ length apiKeys `shouldBe` 1
