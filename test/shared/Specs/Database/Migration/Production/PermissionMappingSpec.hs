module Specs.Database.Migration.Production.PermissionMappingSpec where

import Data.Char (isAlpha)
import qualified Data.List as L
import Test.Hspec

import Shared.Model.User.RolePermission
import WizardServer.Database.Migration.Production.Migration_5_0_0.Migration

permissionMappingSpec =
  describe "Migration 5.0.0 permission mapping" $ do
    it "renamePermissionSql maps an old permission to its first new name" $ do
      renamePermissionSql "c" `shouldContain` "WHEN 'SettingsManageRolePermission' THEN 'organizationSettings.manage'"
      renamePermissionSql "c" `shouldContain` "WHEN 'IntegrationHubUseRolePermission' THEN 'knowledgeModels.manage'"
    it "renamePermissionsSql splits the old settings and users permissions" $ do
      renamePermissionsSql "c" `shouldContain` "ARRAY['organizationSettings.manage', 'settings.manage', 'roles.manage']"
      renamePermissionsSql "c" `shouldContain` "ARRAY['users.manage', 'members.manage', 'userGroups.manage']"
    it "renamePermissionsSql grants projects.create to everyone" $
      renamePermissionsSql "c" `shouldContain` "'projects.create'"
    it "renamePermissionsSql produces known permissions only" $ do
      newPermissions (renamePermissionsSql "c") `shouldSatisfy` (not . null)
      filter (`notElem` knownPermissions) (newPermissions (renamePermissionsSql "c")) `shouldBe` []

knownPermissions :: [String]
knownPermissions =
  allRolePermissions
    ++ ["hubspot.use", "analytics.use", "auditLog.use", "automations.manage", "mcp.connect", "dev.use", "tenants.manage"]

newPermissions :: String -> [String]
newPermissions = filter (not . L.isSuffixOf "RolePermission") . filter isPermissionLike . quoted

quoted :: String -> [String]
quoted sql =
  case dropWhile (/= '\'') sql of
    [] -> []
    (_ : rest) ->
      let (value, rest') = break (== '\'') rest
       in value : quoted (drop 1 rest')

isPermissionLike :: String -> Bool
isPermissionLike value = not (null value) && all (\c -> isAlpha c || c == '.') value
