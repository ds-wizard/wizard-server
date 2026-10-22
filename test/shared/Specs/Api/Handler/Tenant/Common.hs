module Specs.Api.Handler.Tenant.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Model.Tenant.Tenant
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfAppInDB requestContext tenant = do
  tenantFromDb <- getOneFromDB (findTenantByUuid tenant.uuid) requestContext
  compareTenantDtos tenantFromDb tenant

assertAbsenceOfTenantInDB requestContext tenant = do
  mTenantFromDb <- getOneFromDB (findTenantByUuid' tenant.uuid) requestContext
  liftIO $ fmap (.uuid) mTenantFromDb `shouldBe` Nothing

assertAbsenceOfTenantDataInDB requestContext tenant = do
  let tenantRequestContext = requestContext {currentTenantUuid = tenant.uuid}
  assertCountInDB findDocuments tenantRequestContext 0
  assertCountInDB findLocales tenantRequestContext 0

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
