module Specs.Service.Workspace.WorkspaceScopeServiceSpec where

import Control.Monad.Reader (local)
import Test.Hspec

import Shared.Constant.Workspace
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Context.Scope
import Shared.Model.Error.Error
import Shared.Model.Workspace.Workspace
import Shared.Service.Workspace.WorkspaceScopeService
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Workspace.Common
import Specs.Common

workspaceScopeServiceSpec requestContext =
  describe "WorkspaceScopeService" $ do
    resolveScopeSpec requestContext
    requireWorkspaceScopeSpec requestContext
    requirePlaneScopeSpec requestContext
    planeOfWorkspaceSpec requestContext
    resolveCommandWorkspaceUuidSpec requestContext

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
      runInContextIO (insertWorkspace secondWorkspace) requestContext
      runInContextIO (insertWorkspaceMembership (secondWorkspaceMembership userCharles)) requestContext
      result <- runInContext (inScope (WorkspaceScope secondWorkspace.uuid) True requireWorkspaceScope) requestContext
      isNotExistsError result `shouldBe` True
    it "multi-workspace tenant lets an anonymous caller use an existing workspace" $
      runInContext (anonymous (inScope (WorkspaceScope defaultWorkspaceUuid) True requireWorkspaceScope)) requestContext
        >>= (`shouldBe` Right defaultWorkspaceUuid)
    it "multi-workspace tenant hides a non-existing workspace from an anonymous caller as 404" $ do
      result <- runInContext (anonymous (inScope (WorkspaceScope differentWorkspaceUuid) True requireWorkspaceScope)) requestContext
      isNotExistsError result `shouldBe` True

requirePlaneScopeSpec requestContext =
  describe "requirePlaneScope" $ do
    it "single-workspace tenant defaults to the tenant plane" $
      runInContext requirePlaneScope requestContext >>= (`shouldBe` Right Nothing)
    it "multi-workspace tenant requires a plane" $
      runInContext (inScope NoScope True requirePlaneScope) requestContext >>= (`shouldBe` Left (UserError _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED))
    it "tenant=true is the tenant plane" $
      runInContext (inScope TenantScope False requirePlaneScope) requestContext >>= (`shouldBe` Right Nothing)
    it "single-workspace tenant refuses w" $
      runInContext (inScope (WorkspaceScope defaultWorkspaceUuid) False requirePlaneScope) requestContext
        >>= (`shouldBe` Left (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED))
    it "multi-workspace tenant accepts w" $
      runInContext (inScope (WorkspaceScope defaultWorkspaceUuid) True requirePlaneScope) requestContext
        >>= (`shouldBe` Right (Just defaultWorkspaceUuid))

planeOfWorkspaceSpec requestContext =
  describe "planeOfWorkspace" $ do
    it "single-workspace tenant is the tenant plane" $
      runInContext (planeOfWorkspace defaultWorkspaceUuid) requestContext >>= (`shouldBe` Right Nothing)
    it "multi-workspace tenant is the workspace plane" $
      runInContext (inScope NoScope True (planeOfWorkspace defaultWorkspaceUuid)) requestContext >>= (`shouldBe` Right (Just defaultWorkspaceUuid))

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

inScope :: Scope -> Bool -> RequestContextM a -> RequestContextM a
inScope scope multiWorkspace = local (\context -> context {scope = scope, tenantMultiWorkspace = multiWorkspace})

anonymous :: RequestContextM a -> RequestContextM a
anonymous = local (\context -> context {currentUser = Nothing})

isNotExistsError :: Either AppError a -> Bool
isNotExistsError (Left (NotExistsError _)) = True
isNotExistsError _ = False
