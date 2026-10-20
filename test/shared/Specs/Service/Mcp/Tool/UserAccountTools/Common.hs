module Specs.Service.Mcp.Tool.UserAccountTools.Common where

import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Tenant
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Shared.Model.User.User
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Model.UserEmailLink.UserEmailLinkType
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
newPassword :: String
newPassword = "Quiet-Lantern-Harbor-73"

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfForgottenPasswordLinkInDB :: RequestContext -> User -> WaiSession () ()
assertExistenceOfForgottenPasswordLinkInDB requestContext user = do
  Just link <- findForgottenPasswordLink requestContext user
  liftIO $ link `shouldBe` link {identity = user.uuid, aType = ForgottenPasswordUserEmailLinkType, tenantUuid = defaultTenantUuid}

assertAbsenceOfForgottenPasswordLinkInDB :: RequestContext -> User -> WaiSession () ()
assertAbsenceOfForgottenPasswordLinkInDB requestContext user = do
  mLink <- findForgottenPasswordLink requestContext user
  liftIO $ mLink `shouldBe` Nothing

findForgottenPasswordLink :: RequestContext -> User -> WaiSession () (Maybe (UserEmailLink U.UUID UserEmailLinkType))
findForgottenPasswordLink requestContext user = getOneFromDB (findUserEmailLinkByIdentityAndType' (U.toString user.uuid) ForgottenPasswordUserEmailLinkType) requestContext
