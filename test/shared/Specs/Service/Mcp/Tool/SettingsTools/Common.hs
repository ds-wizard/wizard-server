module Specs.Service.Mcp.Tool.SettingsTools.Common where

import Data.Aeson (Value (..))
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Tenant
import Shared.Database.DAO.Settings.SettingsAuthenticationDAO
import Shared.Database.DAO.Settings.SettingsProjectsDAO
import Shared.Database.DAO.Settings.SettingsRolesDAO
import Shared.Database.DAO.Settings.SettingsSubmissionDAO
import Shared.Database.DAO.Settings.SettingsSupportDAO
import Shared.Model.Settings.Settings
import Shared.Service.Settings.OrganizationSettingsService
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
sectionNames :: [String]
sectionNames = ["authentication", "users", "roles", "login-screen", "features", "look-and-feel", "dashboard-and-menu", "projects", "support", "submission", "registry"]

settingsToolNames :: [Value]
settingsToolNames = [String "get_settings", String "update_settings"]

maskedSubmission :: SettingsSubmission -> SettingsSubmission
maskedSubmission settings = settings {services = fmap (\service -> withHeaders [(header, "********") | header <- M.keys service.request.headers] service) settings.services}

withRequestUrl :: String -> SettingsSubmissionService -> SettingsSubmissionService
withRequestUrl url service = service {request = service.request {url = url}}

withHeaders :: [(String, String)] -> SettingsSubmissionService -> SettingsSubmissionService
withHeaders headers service = service {request = service.request {headers = M.fromList headers}}

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertSettingsProjectsInDB :: RequestContext -> Maybe U.UUID -> Maybe SettingsProjects -> WaiSession () ()
assertSettingsProjectsInDB requestContext mWorkspaceUuid expSettings = do
  settings <- getOneFromDB (findSettingsProjects defaultTenantUuid mWorkspaceUuid) requestContext
  liftIO $ settings `shouldBe` expSettings

assertSettingsSupportInDB :: RequestContext -> Maybe U.UUID -> Maybe SettingsSupport -> WaiSession () ()
assertSettingsSupportInDB requestContext mWorkspaceUuid expSettings = do
  settings <- getOneFromDB (findSettingsSupport defaultTenantUuid mWorkspaceUuid) requestContext
  liftIO $ settings `shouldBe` expSettings

assertSettingsRolesInDB :: RequestContext -> SettingsRoles -> WaiSession () ()
assertSettingsRolesInDB requestContext expSettings = do
  settings <- getOneFromDB (findSettingsRoles defaultTenantUuid) requestContext
  liftIO $ settings `shouldBe` Just expSettings

assertSettingsSubmissionInDB :: RequestContext -> SettingsSubmission -> WaiSession () ()
assertSettingsSubmissionInDB requestContext expSettings = do
  settings <- getOneFromDB (findSettingsSubmission defaultTenantUuid Nothing) requestContext
  liftIO $ settings `shouldBe` Just expSettings

assertSettingsRegistryInDB :: RequestContext -> SettingsRegistry -> WaiSession () ()
assertSettingsRegistryInDB requestContext expSettings = do
  settings <- getOneFromDB getCurrentSettingsRegistry requestContext
  liftIO $ settings `shouldBe` expSettings

assertSettingsAuthenticationInDB :: RequestContext -> SettingsAuthentication -> WaiSession () ()
assertSettingsAuthenticationInDB requestContext expSettings = do
  settings <- getOneFromDB (findSettingsAuthentication defaultTenantUuid) requestContext
  liftIO $ settings `shouldBe` Just expSettings
