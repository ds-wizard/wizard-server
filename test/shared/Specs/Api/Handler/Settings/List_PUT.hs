module Specs.Api.Handler.Settings.List_PUT (
  list_PUT,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Constant.Workspace
import Shared.Database.DAO.Settings.SettingsAuthenticationDAO
import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.DAO.Settings.SettingsProjectsDAO
import Shared.Database.DAO.Settings.SettingsRegistryDAO
import Shared.Database.DAO.Settings.SettingsRolesDAO
import Shared.Database.DAO.Settings.SettingsSubmissionDAO as Submission
import Shared.Database.DAO.Settings.SettingsSupportDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.Roles
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Settings.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.Settings.Settings
import Shared.Model.Tenant.Tenant
import Shared.Model.User.Role
import Shared.Model.Workspace.Workspace
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Settings.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/settings/{section}
-- ------------------------------------------------------------------------
list_PUT :: RequestContext -> SpecWith ((), Application)
list_PUT requestContext =
  describe "PUT /api/settings/{section}" $ do
    test_200_tenant requestContext
    test_200_registry requestContext
    test_200_roles requestContext
    test_200_workspace requestContext
    test_200_workspace_role requestContext
    test_200_override_allowed_ignored requestContext
    test_200_override_not_allowed_deletes_overrides requestContext
    test_200_override_not_allowed_deletes_children requestContext
    test_400_override_not_allowed requestContext
    test_400_roles_plane requestContext
    test_400_single_workspace requestContext
    test_400_roles_single_workspace requestContext
    test_400_organization_only requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_roles requestContext
    test_403_workspace requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

editedAuthentication = settingsAuthentication {registrationEnabled = False} :: SettingsAuthentication

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_tenant requestContext = do
  it "HTTP 200 OK (authentication, tenant=true)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantUrl "authentication") reqHeaders (settingsBody editedAuthentication Nothing)
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO editedAuthentication Nothing Nothing) response
    -- AND: Find result in DB and compare with expectation state
    settings <- getOneFromDB (findSettingsAuthentication defaultTenant.uuid) requestContext
    liftIO $ settings `shouldBe` Just editedAuthentication
  it "HTTP 200 OK (projects, tenant=true)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantUrl "projects") reqHeaders (settingsBody editedSettingsProjects Nothing)
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO editedSettingsProjects Nothing Nothing) response
    -- AND: Find result in DB and compare with expectation state
    settings <- getOneFromDB (findSettingsProjects defaultTenant.uuid Nothing) requestContext
    liftIO $ settings `shouldBe` Just editedSettingsProjects

test_200_registry requestContext =
  it "HTTP 200 OK (registry stores the API key encrypted)" $ do
    -- GIVEN: Prepare expectation
    let reqValue = settingsRegistry {apiKey = "NewApiKey"} :: SettingsRegistry
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantUrl "registry") reqHeaders (settingsBody reqValue Nothing)
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO reqValue Nothing Nothing) response
    -- AND: Find result in DB and compare with expectation state
    mSettings <- getOneFromDB (findSettingsRegistry defaultTenant.uuid) requestContext
    liftIO $ fmap (.apiKey) mSettings `shouldNotBe` Just "NewApiKey"

test_200_roles requestContext = do
  it "HTTP 200 OK (roles, tenant=true)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantUrl "roles") reqHeaders (settingsBody (SettingsRoles dataStewardRole.uuid) Nothing)
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO (SettingsRoles dataStewardRole.uuid) Nothing Nothing) response
    -- AND: Find result in DB and compare with expectation state
    settings <- getOneFromDB (findSettingsRoles defaultTenant.uuid) requestContext
    liftIO $ settings `shouldBe` Just (SettingsRoles dataStewardRole.uuid)
  it "HTTP 200 OK (roles, w writes the default role of the workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "roles") reqHeaders (settingsBody (SettingsRoles defaultWorkspaceAdminRole.uuid) Nothing)
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO (SettingsRoles defaultWorkspaceAdminRole.uuid) Nothing Nothing) response
    -- AND: Find result in DB and compare with expectation state
    workspace <- getOneFromDB (findWorkspaceByUuid defaultWorkspaceUuid) requestContext
    liftIO $ workspace.defaultRoleUuid `shouldBe` Just defaultWorkspaceAdminRole.uuid

test_200_workspace requestContext =
  it "HTTP 200 OK (w creates the override)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders (settingsBody editedSettingsSupport (Just False))
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO editedSettingsSupport (Just True) (Just True)) response
    -- AND: Find result in DB and compare with expectation state
    override <- getOneFromDB (findSettingsSupport defaultTenant.uuid (Just defaultWorkspaceUuid)) requestContext
    liftIO $ override `shouldBe` Just editedSettingsSupport
    org <- getOneFromDB (findSettingsSupport defaultTenant.uuid Nothing) requestContext
    liftIO $ org `shouldBe` Just settingsSupport

test_200_override_not_allowed_deletes_overrides requestContext =
  it "HTTP 200 OK (overrideAllowed false deletes the workspace overrides)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO (saveSettingsSupport defaultTenant.uuid (Just defaultWorkspaceUuid) editedSettingsSupport) requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantUrl "support") reqHeaders (settingsBody settingsSupport (Just False))
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO settingsSupport (Just False) Nothing) response
    -- AND: Find result in DB and compare with expectation state
    override <- getOneFromDB (findSettingsSupport defaultTenant.uuid (Just defaultWorkspaceUuid)) requestContext
    liftIO $ override `shouldBe` Nothing

test_200_workspace_role requestContext =
  it "HTTP 200 OK (w with settings.manage on the workspace role)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceSettingsManager requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders (settingsBody editedSettingsSupport Nothing)
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO editedSettingsSupport (Just True) (Just True)) response

test_200_override_allowed_ignored requestContext =
  it "HTTP 200 OK (overrideAllowed is ignored in a single-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantUrl "support") reqHeaders (settingsBody settingsSupport (Just False))
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO settingsSupport Nothing Nothing) response
    -- AND: Find result in DB and compare with expectation state
    overrideAllowed <- getOneFromDB (findSettingsOverrideAllowed "settings_support" defaultTenant.uuid) requestContext
    liftIO $ overrideAllowed `shouldBe` Just True

test_200_override_not_allowed_deletes_children requestContext =
  it "HTTP 200 OK (overrideAllowed false deletes the overrides of every workspace with their child rows)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    runInContextIO (saveSettingsSubmission defaultTenant.uuid (Just defaultWorkspaceUuid) settingsSubmission) requestContext
    runInContextIO (saveSettingsSubmission defaultTenant.uuid (Just secondWorkspaceUuid) settingsSubmission) requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantUrl "submission") reqHeaders (settingsBody settingsSubmissionEmpty (Just False))
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO settingsSubmissionEmpty (Just False) Nothing) response
    -- AND: Find result in DB and compare with expectation state
    counts <- traverse (`countWorkspaceRows` requestContext) (Submission.table : Submission.childTables)
    liftIO $ counts `shouldBe` [0, 0, 0, 0]

test_400_override_not_allowed requestContext =
  it "HTTP 400 BAD REQUEST (w when the organization does not allow overrides)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO (updateSettingsOverrideAllowed "settings_support" defaultTenant.uuid False) requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders (settingsBody editedSettingsSupport Nothing)
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_SETTINGS__OVERRIDE_NOT_ALLOWED) response

test_400_roles_plane requestContext = do
  it "HTTP 400 BAD REQUEST (roles, tenant=true with a workspace role)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantUrl "roles") reqHeaders (settingsBody (SettingsRoles defaultWorkspaceUserRole.uuid) Nothing)
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_VALIDATION__USER_ROLE_NOT_ORGANIZATION) response
  it "HTTP 400 BAD REQUEST (roles, w with an organization role)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "roles") reqHeaders (settingsBody (SettingsRoles researcherRole.uuid) Nothing)
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_SETTINGS__ROLE_NOT_IN_WORKSPACE) response

test_400_single_workspace requestContext =
  it "HTTP 400 BAD REQUEST (w in a single-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "projects") reqHeaders (settingsBody editedSettingsProjects Nothing)
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE) response

test_400_roles_single_workspace requestContext =
  it "HTTP 400 BAD REQUEST (roles, w in a single-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "roles") reqHeaders (settingsBody (SettingsRoles defaultWorkspaceAdminRole.uuid) Nothing)
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE) response

test_400_organization_only requestContext =
  it "HTTP 400 BAD REQUEST (w on an organization-only section)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "features") reqHeaders (settingsBody settingsFeatures Nothing)
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_SETTINGS__ORGANIZATION_ONLY) response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod (tenantUrl "support") [reqCtHeader] (settingsBody settingsSupport Nothing)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod (tenantUrl "support") [reqCtHeader] (settingsBody settingsSupport Nothing) "organizationSettings.manage"
test_403_roles requestContext = createNoPermissionTest requestContext reqMethod (tenantUrl "roles") [reqCtHeader] (settingsBody (SettingsRoles dataStewardRole.uuid) Nothing) "roles.manage"

test_403_workspace requestContext =
  it "HTTP 403 FORBIDDEN (w without settings.manage in that workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceUserRole requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders (settingsBody editedSettingsSupport Nothing)
    -- THEN: Compare response with expectation
    expectStatus 403 response
