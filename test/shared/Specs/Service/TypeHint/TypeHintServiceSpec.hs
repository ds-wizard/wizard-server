module Specs.Service.TypeHint.TypeHintServiceSpec where

import qualified Data.Map.Strict as M
import Test.Hspec

import Shared.Constant.Workspace
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Model.Workspace.Workspace
import Shared.Service.TypeHint.TypeHintService

import Specs.Common

typeHintServiceSpec requestContext =
  describe "TypeHintService" $
    describe "prepareSecrets" $ do
      it "the workspace secret overrides the tenant secret of the same name" $ do
        -- GIVEN: Secrets in the tenant plane and in two workspaces
        insertSecrets requestContext
        -- WHEN:
        result <- runInContext (prepareSecrets (Just defaultWorkspaceUuid)) requestContext
        -- THEN:
        fmap (M.lookup kmSecret1.name) result `shouldBe` Right (Just workspaceKmSecret1.value)
        fmap (M.lookup secondWorkspaceKmSecret.name) result `shouldBe` Right Nothing
      it "the tenant plane sees the tenant secrets only" $ do
        -- GIVEN: Secrets in the tenant plane and in two workspaces
        insertSecrets requestContext
        -- WHEN:
        result <- runInContext (prepareSecrets Nothing) requestContext
        -- THEN:
        fmap (M.lookup kmSecret1.name) result `shouldBe` Right (Just kmSecret1.value)
        fmap (M.lookup secondWorkspaceKmSecret.name) result `shouldBe` Right Nothing

insertSecrets requestContext = do
  runInContextIO (insertWorkspace (secondWorkspace {defaultRoleUuid = Nothing} :: Workspace)) requestContext
  runInContextIO (insertRole secondWorkspaceAdminRole) requestContext
  runInContextIO (insertRole secondWorkspaceUserRole) requestContext
  runInContextIO (updateWorkspaceByUuid secondWorkspace) requestContext
  runInContextIO (insertKnowledgeModelSecret kmSecret1) requestContext
  runInContextIO (insertKnowledgeModelSecret workspaceKmSecret1) requestContext
  runInContextIO (insertKnowledgeModelSecret secondWorkspaceKmSecret) requestContext
