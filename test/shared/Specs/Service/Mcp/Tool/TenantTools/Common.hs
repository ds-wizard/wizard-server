module Specs.Service.Mcp.Tool.TenantTools.Common where

import Data.Aeson
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Tenant.TenantLimitBundleDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Model.Tenant.Limit.TenantLimitBundle
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
newTenantUuid :: U.UUID
newTenantUuid = u' "5b8e2d4a-7c1f-4a9e-b3d6-0e2f4a6c8b13"

newTenantArguments :: [(Key, Value)]
newTenantArguments =
  [ "uuid" .= newTenantUuid
  , "tenantId" .= ("mcp-tenant" :: String)
  , "name" .= ("MCP Tenant" :: String)
  , "admin" .= object ["firstName" .= ("Ada" :: String), "lastName" .= ("Lovelace" :: String), "email" .= ("ada.lovelace@example.com" :: String)]
  ]

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertTenantInDB :: RequestContext -> U.UUID -> Maybe (String, String) -> WaiSession () ()
assertTenantInDB requestContext uuid expTenant = do
  mTenant <- getOneFromDB (findTenantByUuid' uuid) requestContext
  liftIO $ fmap (\tenant -> (tenant.tenantId, tenant.name)) mTenant `shouldBe` expTenant

assertTenantAdminsInDB :: RequestContext -> U.UUID -> [String] -> WaiSession () ()
assertTenantAdminsInDB requestContext tenantUuid expEmails = do
  users <- getOneFromDB (findUsersWithTenantFiltered tenantUuid []) requestContext
  liftIO $ fmap (.email) users `shouldBe` expEmails

assertTenantLimitBundleInDB :: RequestContext -> TenantLimitBundle -> WaiSession () ()
assertTenantLimitBundleInDB requestContext expLimitBundle = do
  limitBundle <- getOneFromDB (findLimitBundleByUuid expLimitBundle.uuid) requestContext
  liftIO $ limitBundle `shouldBe` expLimitBundle {createdAt = limitBundle.createdAt, updatedAt = limitBundle.updatedAt}
