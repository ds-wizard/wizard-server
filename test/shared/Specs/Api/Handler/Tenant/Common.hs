module Specs.Api.Handler.Tenant.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Model.Tenant.Tenant

import Specs.Api.Handler.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfAppInDB requestContext tenant = do
  tenantFromDb <- getOneFromDB (findTenantByUuid tenant.uuid) requestContext
  compareTenantDtos tenantFromDb tenant

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareTenantDtos resDto expDto = do
  liftIO $ resDto.tenantId `shouldBe` expDto.tenantId
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.serverDomain `shouldBe` expDto.serverDomain
  liftIO $ resDto.serverUrl `shouldBe` expDto.serverUrl
  liftIO $ resDto.clientUrl `shouldBe` expDto.clientUrl
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
