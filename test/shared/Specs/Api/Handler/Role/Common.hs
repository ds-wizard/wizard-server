module Specs.Api.Handler.Role.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.User.RoleDAO
import Shared.Model.User.Role
import Shared.Model.User.RoleList
import WizardServer.Model.Context.RequestContext ()

import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfRoleInDB requestContext role = do
  eRole <- runInContextIO (findRoleByUuid role.uuid) requestContext
  liftIO $ isRight eRole `shouldBe` True
  let (Right roleFromDB) = eRole
  compareRoles roleFromDB role

assertAbsenceOfRoleInDB requestContext roleUuid = do
  roles <- runInContextIO findRoles requestContext
  let exists = either (const False) (any (\r -> r.uuid == roleUuid)) roles
  liftIO $ exists `shouldBe` False

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareRoles :: Role -> Role -> WaiSession st ()
compareRoles resModel expModel = do
  liftIO $ resModel.uuid `shouldBe` expModel.uuid
  liftIO $ resModel.name `shouldBe` expModel.name
  liftIO $ resModel.permissions `shouldBe` expModel.permissions
  liftIO $ resModel.isAdmin `shouldBe` expModel.isAdmin
  liftIO $ resModel.tenantUuid `shouldBe` expModel.tenantUuid

compareRoleDtos :: RoleList -> RoleList -> WaiSession st ()
compareRoleDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.permissions `shouldBe` expDto.permissions
  liftIO $ resDto.usersCount `shouldBe` expDto.usersCount
  liftIO $ resDto.isAdmin `shouldBe` expDto.isAdmin
