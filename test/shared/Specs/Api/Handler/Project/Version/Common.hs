module Specs.Api.Handler.Project.Version.Common where

import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Tenant.Tenant

import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfProjectVersionInDB requestContext version = do
  eVersion <- runInContextIO (findProjectVersionByUuid version.uuid) requestContext
  liftIO $ isRight eVersion `shouldBe` True
  let (Right versionFromDb) = eVersion
  compareProjectVersionCreateDtos versionFromDb version

assertAbsenceOfProjectVersionInDB requestContext version = do
  eVersion <- runInContextIO (findProjectVersionByUuid version.uuid) requestContext
  liftIO $ isLeft eVersion `shouldBe` True
  let (Left error) = eVersion
  liftIO $
    error
      `shouldBe` NotExistsError
        (_ERROR_DATABASE__ENTITY_NOT_FOUND "project_version" [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString version.uuid)])

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareProjectVersionCreateDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.eventUuid `shouldBe` expDto.eventUuid
