module Specs.Api.Handler.KnowledgeModelPackage.Common where

import Control.Monad (forM_)

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

import Specs.Api.Handler.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfPackageInDB requestContext package = do
  packageFromDb <- getOneFromDB (findPackageByUuid package.uuid) requestContext
  comparePackageDtos packageFromDb package

assertExistenceOfBundlePackageInDB requestContext package = do
  packageFromDb <- getOneFromDB (findPackageByCoordinate (Coordinate package.organizationId package.kmId package.version)) requestContext
  comparePackageDtos packageFromDb package

-- --------------------------------
-- COMPARATORS
-- --------------------------------
comparePackageDtosList res exp =
  forM_ (zip res exp) $ uncurry comparePackageDtos

comparePackageDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.organizationId `shouldBe` expDto.organizationId
  liftIO $ resDto.kmId `shouldBe` expDto.kmId
  liftIO $ resDto.version `shouldBe` expDto.version
  liftIO $ resDto.description `shouldBe` expDto.description
