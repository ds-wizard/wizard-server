module Specs.Service.Workspace.CoordinateResolutionSpec where

import Test.Hspec

import Shared.Constant.Workspace
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Util.Uuid

import Specs.Common

coordinateResolutionSpec requestContext =
  describe "Coordinate resolution across planes" $ do
    it "the tenant row is found without a workspace" $ do
      result <- runInContext (findPackageByCoordinate' (createCoordinate netherlandsKmPackage) Nothing) requestContext
      fmap (fmap (.uuid)) result `shouldBe` Right (Just netherlandsKmPackage.uuid)
    it "the workspace row wins the tie with the tenant row" $ do
      runInContextIO (insertPackage workspaceNetherlandsKmPackage) requestContext
      result <- runInContext (findPackageByCoordinate' (createCoordinate netherlandsKmPackage) (Just defaultWorkspaceUuid)) requestContext
      fmap (fmap (.uuid)) result `shouldBe` Right (Just workspaceNetherlandsKmPackage.uuid)
    it "the newest version comes from the tenant and the workspace rows together" $ do
      runInContextIO (insertPackage workspaceNetherlandsKmPackageV3) requestContext
      tenantResult <- runInContext (findLatestPackageById' "org.nl.core-nl" Nothing Nothing) requestContext
      fmap (fmap (.uuid)) tenantResult `shouldBe` Right (Just netherlandsKmPackageV2.uuid)
      workspaceResult <- runInContext (findLatestPackageById' "org.nl.core-nl" Nothing (Just defaultWorkspaceUuid)) requestContext
      fmap (fmap (.uuid)) workspaceResult `shouldBe` Right (Just workspaceNetherlandsKmPackageV3.uuid)
    it "a foreign workspace sees only the tenant rows" $ do
      runInContextIO (insertPackage workspaceNetherlandsKmPackageV3) requestContext
      result <- runInContext (findLatestPackageById' "org.nl.core-nl" Nothing (Just differentWorkspaceUuid)) requestContext
      fmap (fmap (.uuid)) result `shouldBe` Right (Just netherlandsKmPackageV2.uuid)

workspaceNetherlandsKmPackage :: KnowledgeModelPackage
workspaceNetherlandsKmPackage =
  netherlandsKmPackage
    { uuid = u' "0d7b8f5e-2c4a-4e1b-9f3d-6a5c4b3e2d1f"
    , workspaceUuid = Just defaultWorkspaceUuid
    }

workspaceNetherlandsKmPackageV3 :: KnowledgeModelPackage
workspaceNetherlandsKmPackageV3 =
  netherlandsKmPackageV2
    { uuid = u' "1e8c9a6f-3d5b-4f2c-8a4e-7b6d5c4f3e2a"
    , version = "3.0.0"
    , previousPackageUuid = Nothing
    , workspaceUuid = Just defaultWorkspaceUuid
    }
