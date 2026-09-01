module Specs.Service.User.UserServiceSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Public
import Shared.Model.Config.SimpleFeature
import Shared.Model.Error.Error
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Service.Tenant.Config.ConfigService
import Shared.Service.User.UserService

import Specs.Common

userServiceIntegrationSpec requestContext =
  describe "User Service Integration" $
    describe "registerUser" $
      it "Registration is disabled" $
        -- GIVEN: Prepare expectations
        do
          let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registration"
          -- AND: Update config in DB
          (Right tcAuthentication) <- runInContext getCurrentTenantConfigAuthentication requestContext
          let tcAuthenticationUpdated = tcAuthentication {internal = tcAuthentication.internal {registration = tcAuthentication.internal.registration {enabled = False}}}
          tcAuthentication <- runInContext (modifyTenantConfigAuthentication tcAuthenticationUpdated) requestContext
          -- WHEN:
          result <- runInContext (registerUser userJohnCreate) requestContext
          -- THEN:
          result `shouldBe` expectation
