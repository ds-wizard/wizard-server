module Specs.Service.Workspace.WorkspaceScopeServiceSpec where

import Control.Monad.Reader (local)
import Test.Hspec

import Shared.Api.Resource.User.UserDTO
import Shared.Constant.Workspace
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Context.Scope
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Service.User.WizardUserMapper (toDTO)
import Shared.Service.Workspace.WorkspaceScopeService
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Workspace.Common
import Specs.Common

workspaceScopeServiceSpec requestContext =
  describe "WorkspaceScopeService" $ do
    resolveScopeSpec requestContext
    requireWorkspaceScopeSpec requestContext
    requireTenantOrWorkspaceScopeSpec requestContext
    ownerWorkspaceUuidSpec requestContext
    resolveCommandWorkspaceUuidSpec requestContext
    isWorkspaceReachableSpec requestContext
    checkWorkspaceMatchSpec requestContext

resolveScopeSpec requestContext =
  describe "resolveScope" $ do
    it "no parameter is NoScope" $
      runInContext (resolveScope Nothing Nothing) requestContext >>= (`shouldBe` Right NoScope)
    it "tenant=true is TenantScope" $
      runInContext (resolveScope Nothing (Just True)) requestContext >>= (`shouldBe` Right TenantScope)
    it "tenant=false is NoScope" $
      runInContext (resolveScope Nothing (Just False)) requestContext >>= (`shouldBe` Right NoScope)
    it "w is WorkspaceScope" $
      runInContext (resolveScope (Just defaultWorkspaceUuid) Nothing) requestContext >>= (`shouldBe` Right (WorkspaceScope defaultWorkspaceUuid))
    it "w and tenant=true is 400" $
      runInContext (resolveScope (Just defaultWorkspaceUuid) (Just True)) requestContext >>= (`shouldBe` Left (UserError _ERROR_SERVICE_WORKSPACE__SCOPE_CONFLICT))

requireWorkspaceScopeSpec requestContext =
  describe "requireWorkspaceScope" $ do
    it "single-workspace tenant assigns the only workspace" $
      runInContext requireWorkspaceScope requestContext >>= (`shouldBe` Right defaultWorkspaceUuid)
    it "single-workspace tenant hides the only workspace from a non-member as 404" $ do
      runInContextIO (deleteWorkspaceMembership defaultWorkspaceUuid userAlbert.uuid) requestContext
      result <- runInContext (asResearcher requireWorkspaceScope) requestContext
      isNotExistsError result `shouldBe` True
    it "single-workspace tenant refuses w" $
      runInContext (inScope (WorkspaceScope defaultWorkspaceUuid) False requireWorkspaceScope) requestContext
        >>= (`shouldBe` Left (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED))
    it "tenant=true is refused" $
      runInContext (inScope TenantScope False requireWorkspaceScope) requestContext
        >>= (`shouldBe` Left (UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED))
    it "multi-workspace tenant requires w" $
      runInContext (inScope NoScope True requireWorkspaceScope) requestContext
        >>= (`shouldBe` Left (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED))
    it "multi-workspace tenant accepts a workspace the caller is a member of" $
      runInContext (inScope (WorkspaceScope defaultWorkspaceUuid) True requireWorkspaceScope) requestContext
        >>= (`shouldBe` Right defaultWorkspaceUuid)
    it "multi-workspace tenant hides a foreign workspace as 404" $ do
      insertSecondWorkspaceWithoutMembers requestContext
      result <- runInContext (asResearcher (inScope (WorkspaceScope secondWorkspace.uuid) True requireWorkspaceScope)) requestContext
      isNotExistsError result `shouldBe` True
    it "multi-workspace tenant lets an anonymous caller use an existing workspace" $
      runInContext (anonymous (inScope (WorkspaceScope defaultWorkspaceUuid) True requireWorkspaceScope)) requestContext
        >>= (`shouldBe` Right defaultWorkspaceUuid)
    it "multi-workspace tenant hides a non-existing workspace from an anonymous caller as 404" $ do
      result <- runInContext (anonymous (inScope (WorkspaceScope differentWorkspaceUuid) True requireWorkspaceScope)) requestContext
      isNotExistsError result `shouldBe` True

requireTenantOrWorkspaceScopeSpec requestContext =
  describe "requireTenantOrWorkspaceScope" $ do
    it "single-workspace tenant defaults to the tenant plane" $
      runInContext requireTenantOrWorkspaceScope requestContext >>= (`shouldBe` Right Nothing)
    it "multi-workspace tenant requires a plane" $
      runInContext (inScope NoScope True requireTenantOrWorkspaceScope) requestContext >>= (`shouldBe` Left (UserError _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED))
    it "tenant=true is the tenant plane" $
      runInContext (inScope TenantScope False requireTenantOrWorkspaceScope) requestContext >>= (`shouldBe` Right Nothing)
    it "single-workspace tenant refuses w" $
      runInContext (inScope (WorkspaceScope defaultWorkspaceUuid) False requireTenantOrWorkspaceScope) requestContext
        >>= (`shouldBe` Left (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED))
    it "multi-workspace tenant accepts w" $
      runInContext (inScope (WorkspaceScope defaultWorkspaceUuid) True requireTenantOrWorkspaceScope) requestContext
        >>= (`shouldBe` Right (Just defaultWorkspaceUuid))

ownerWorkspaceUuidSpec requestContext =
  describe "ownerWorkspaceUuid" $ do
    it "single-workspace tenant is the tenant plane" $
      runInContext (ownerWorkspaceUuid defaultWorkspaceUuid) requestContext >>= (`shouldBe` Right Nothing)
    it "multi-workspace tenant is the workspace plane" $
      runInContext (inScope NoScope True (ownerWorkspaceUuid defaultWorkspaceUuid)) requestContext >>= (`shouldBe` Right (Just defaultWorkspaceUuid))

resolveCommandWorkspaceUuidSpec requestContext =
  describe "resolveCommandWorkspaceUuid" $ do
    it "single-workspace tenant assigns the only workspace" $
      runInContext (resolveCommandWorkspaceUuid Nothing) requestContext >>= (`shouldBe` Right defaultWorkspaceUuid)
    it "multi-workspace tenant requires a workspace" $ do
      enableMultiWorkspace requestContext
      runInContext (resolveCommandWorkspaceUuid Nothing) requestContext
        >>= (`shouldBe` Left (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED))
    it "an existing workspace is kept" $
      runInContext (resolveCommandWorkspaceUuid (Just defaultWorkspaceUuid)) requestContext >>= (`shouldBe` Right defaultWorkspaceUuid)
    it "a workspace of another tenant is 404" $ do
      result <- runInContext (resolveCommandWorkspaceUuid (Just differentWorkspaceUuid)) requestContext
      isNotExistsError result `shouldBe` True

isWorkspaceReachableSpec requestContext =
  describe "isWorkspaceReachable" $ do
    it "a member reaches the workspace" $
      runInContext (asResearcher (isWorkspaceReachable defaultWorkspaceUuid)) requestContext >>= (`shouldBe` Right True)
    it "an organization role with projects.view reaches the workspace" $ do
      runInContextIO (deleteWorkspaceMembership defaultWorkspaceUuid userAlbert.uuid) requestContext
      runInContext (asRole projectViewerRole (isWorkspaceReachable defaultWorkspaceUuid)) requestContext >>= (`shouldBe` Right True)
    it "a researcher who is not a member does not reach the workspace" $ do
      runInContextIO (deleteWorkspaceMembership defaultWorkspaceUuid userAlbert.uuid) requestContext
      runInContext (asResearcher (isWorkspaceReachable defaultWorkspaceUuid)) requestContext >>= (`shouldBe` Right False)
    it "an anonymous caller reaches the workspace" $
      runInContext (anonymous (isWorkspaceReachable defaultWorkspaceUuid)) requestContext >>= (`shouldBe` Right True)

checkWorkspaceMatchSpec requestContext =
  describe "checkWorkspaceMatch" $ do
    describe "checkPackageWorkspace" $ do
      it "a tenant-plane package passes" $
        runInContext (checkPackageWorkspace defaultWorkspaceUuid Nothing) requestContext >>= (`shouldBe` Right ())
      it "a package of the same workspace passes" $
        runInContext (checkPackageWorkspace defaultWorkspaceUuid (Just defaultWorkspaceUuid)) requestContext >>= (`shouldBe` Right ())
      it "a package of another workspace is 404" $
        runInContext (checkPackageWorkspace defaultWorkspaceUuid (Just secondWorkspaceUuid)) requestContext
          >>= (`shouldBe` Left (NotExistsError (_ERROR_VALIDATION__ABSENCE "knowledge_model_package")))
    describe "checkTemplateWorkspace" $ do
      it "a tenant-plane template passes" $
        runInContext (checkTemplateWorkspace defaultWorkspaceUuid Nothing) requestContext >>= (`shouldBe` Right ())
      it "a template of the same workspace passes" $
        runInContext (checkTemplateWorkspace defaultWorkspaceUuid (Just defaultWorkspaceUuid)) requestContext >>= (`shouldBe` Right ())
      it "a template of another workspace is 404" $
        runInContext (checkTemplateWorkspace defaultWorkspaceUuid (Just secondWorkspaceUuid)) requestContext
          >>= (`shouldBe` Left (NotExistsError (_ERROR_VALIDATION__ABSENCE "document_template")))

inScope :: Scope -> Bool -> RequestContextM a -> RequestContextM a
inScope scope multiWorkspace = local (\context -> context {scope = scope, tenantMultiWorkspace = multiWorkspace})

asResearcher :: RequestContextM a -> RequestContextM a
asResearcher = local (\context -> context {currentUser = Just ((toDTO userAlbert) {role = toRoleSimple researcherRole} :: UserDTO)})

insertSecondWorkspaceWithoutMembers requestContext = do
  runInContextIO (insertWorkspace (secondWorkspace {defaultRoleUuid = Nothing} :: Workspace)) requestContext
  runInContextIO (insertRole secondWorkspaceAdminRole) requestContext
  runInContextIO (insertRole secondWorkspaceUserRole) requestContext
  runInContextIO (updateWorkspaceByUuid secondWorkspace) requestContext

asRole :: Role -> RequestContextM a -> RequestContextM a
asRole role = local (\context -> context {currentUser = Just ((toDTO userAlbert) {role = toRoleSimple role} :: UserDTO)})

projectViewerRole :: Role
projectViewerRole = researcherRole {permissions = [_PROJECTS_VIEW_ROLE_PERMISSION]} :: Role

anonymous :: RequestContextM a -> RequestContextM a
anonymous = local (\context -> context {currentUser = Nothing})

isNotExistsError :: Either AppError a -> Bool
isNotExistsError (Left (NotExistsError _)) = True
isNotExistsError _ = False
