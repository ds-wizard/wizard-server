module Specs.Service.User.UserServiceSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Constant.Tenant
import Shared.Database.DAO.Settings.SettingsAuthenticationDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.OpenId.Data.OpenIdClients
import qualified Shared.Database.Migration.Development.OpenId.OpenIdClientMigration as OpenIdClient
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Settings.Settings
import Shared.Model.Tenant.Tenant
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMembership
import Shared.Service.User.UserService
import WizardServer.Model.Context.RequestContext

import Specs.Common

userServiceIntegrationSpec requestContext =
  describe "User Service Integration" $ do
    describe "createUserFromOpenIdLogin" $ do
      it "Single-workspace tenant: the new user joins the default workspace with its default role" $
        -- GIVEN: Prepare data
        do
          runInContextIO OpenIdClient.runMigration requestContext
          -- WHEN:
          (Right user) <- runInContext createOpenIdUser requestContext
          -- THEN:
          (Right memberships) <- runInContext (findWorkspaceMembershipsByUserUuid user.uuid) requestContext
          fmap (\membership -> (membership.workspaceUuid, membership.roleUuid)) memberships `shouldBe` [(defaultWorkspace.uuid, defaultWorkspaceUserRole.uuid)]
      it "Multi-workspace tenant: the new user joins no workspace" $
        -- GIVEN: Prepare data
        do
          runInContextIO OpenIdClient.runMigration requestContext
          runInContextIO (updateTenantByUuid (defaultTenant {multiWorkspace = True})) requestContext
          -- WHEN:
          (Right user) <- runInContext createOpenIdUser requestContext
          -- THEN:
          (Right memberships) <- runInContext (findWorkspaceMembershipsByUserUuid user.uuid) requestContext
          memberships `shouldBe` []
    describe "registerUser" $
      it "Registration is disabled" $
        -- GIVEN: Prepare expectations
        do
          let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registration"
          -- AND: Update config in DB
          runInContext (saveSettingsAuthentication defaultTenantUuid (settingsAuthentication {registrationEnabled = False} :: SettingsAuthentication)) requestContext
          -- WHEN:
          result <- runInContext (registerUser userJohnCreate) requestContext
          -- THEN:
          result `shouldBe` expectation

createOpenIdUser :: RequestContextM User
createOpenIdUser = createUserFromOpenIdLogin defaultOpenIdClient "openid-external-id" "Open" "Id" "open.id@example.com" Nothing Nothing True
