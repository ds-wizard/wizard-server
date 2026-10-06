module Shared.Service.Settings.SettingsService where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.UUID as U

import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Settings.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Context.AclContext
import Shared.Model.Context.Scope
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Service.Workspace.WorkspaceScopeService

data OverridableSettings m a = OverridableSettings
  { table :: String
  , childTables :: [String]
  , findFn :: U.UUID -> Maybe U.UUID -> m (Maybe a)
  , saveFn :: U.UUID -> Maybe U.UUID -> a -> m ()
  , validateFn :: a -> m ()
  }

requireOrganizationSettingsScope :: WizardRequestContextC s m => m ()
requireOrganizationSettingsScope = do
  scope <- asks (.scope')
  case scope of
    WorkspaceScope _ -> throwError $ UserError _ERROR_SERVICE_SETTINGS__ORGANIZATION_ONLY
    _ -> void requireTenantOrWorkspaceScope

requireSettingsScope :: WizardRequestContextC s m => m (Maybe U.UUID)
requireSettingsScope = do
  scope <- asks (.scope')
  case scope of
    WorkspaceScope workspaceUuid -> do
      checkWorkspacePlaneAvailable
      checkWorkspaceReachable workspaceUuid
      return $ Just workspaceUuid
    _ -> requireTenantOrWorkspaceScope

checkScopedPermission :: WizardRequestContextC s m => String -> String -> Maybe U.UUID -> m ()
checkScopedPermission organizationPermission _ Nothing = checkPermissionInWorkspace organizationPermission Nothing
checkScopedPermission _ workspacePermission mWorkspaceUuid = checkPermissionInWorkspace workspacePermission mWorkspaceUuid

checkSettingsPermission :: WizardRequestContextC s m => Maybe U.UUID -> m ()
checkSettingsPermission = checkScopedPermission _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION _SETTINGS_MANAGE_ROLE_PERMISSION

requireSettings :: WizardRequestContextC s m => Maybe a -> m a
requireSettings = maybe (throwError . NotExistsError $ _ERROR_VALIDATION__ABSENCE "settings") return

getCurrentSettings :: WizardRequestContextC s m => (U.UUID -> m (Maybe a)) -> m a
getCurrentSettings findFn = asks (.tenantUuid') >>= getSettingsByTenantUuid findFn

getSettingsByTenantUuid :: WizardRequestContextC s m => (U.UUID -> m (Maybe a)) -> U.UUID -> m a
getSettingsByTenantUuid findFn tenantUuid = findFn tenantUuid >>= requireSettings

getOrganizationSettings :: WizardRequestContextC s m => (U.UUID -> m (Maybe a)) -> m (SettingsDTO a)
getOrganizationSettings findFn = do
  requireOrganizationSettingsScope
  checkSettingsPermission Nothing
  value <- getCurrentSettings findFn
  return $ SettingsDTO value Nothing Nothing

modifyOrganizationSettings :: WizardRequestContextC s m => (U.UUID -> a -> m ()) -> SettingsDTO a -> m (SettingsDTO a)
modifyOrganizationSettings saveFn reqDto =
  runInTransaction $ do
    requireOrganizationSettingsScope
    checkSettingsPermission Nothing
    tenantUuid <- asks (.tenantUuid')
    saveFn tenantUuid reqDto.value
    return $ SettingsDTO reqDto.value Nothing Nothing

getEffectiveSettings :: WizardRequestContextC s m => OverridableSettings m a -> Maybe U.UUID -> m a
getEffectiveSettings section mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  mOverride <- maybe (return Nothing) (section.findFn tenantUuid . Just) mWorkspaceUuid
  maybe (section.findFn tenantUuid Nothing >>= requireSettings) return mOverride

getOverridableSettings :: WizardRequestContextC s m => OverridableSettings m a -> m (SettingsDTO a)
getOverridableSettings section = do
  mWorkspaceUuid <- requireSettingsScope
  checkSettingsPermission mWorkspaceUuid
  toOverridableSettingsDTO section mWorkspaceUuid

modifyOverridableSettings :: WizardRequestContextC s m => OverridableSettings m a -> SettingsDTO a -> m (SettingsDTO a)
modifyOverridableSettings section reqDto =
  runInTransaction $ do
    mWorkspaceUuid <- requireSettingsScope
    checkSettingsPermission mWorkspaceUuid
    section.validateFn reqDto.value
    tenantUuid <- asks (.tenantUuid')
    case mWorkspaceUuid of
      Nothing -> do
        section.saveFn tenantUuid Nothing reqDto.value
        multiWorkspace <- asks (.tenantMultiWorkspace')
        when multiWorkspace $ forM_ reqDto.overrideAllowed (modifyOverrideAllowed section tenantUuid)
      Just workspaceUuid -> do
        overrideAllowed <- findSettingsOverrideAllowed section.table tenantUuid >>= requireSettings
        unless overrideAllowed (throwError $ UserError _ERROR_SERVICE_SETTINGS__OVERRIDE_NOT_ALLOWED)
        section.saveFn tenantUuid (Just workspaceUuid) reqDto.value
    toOverridableSettingsDTO section mWorkspaceUuid

deleteOverridableSettings :: WizardRequestContextC s m => OverridableSettings m a -> m ()
deleteOverridableSettings section =
  runInTransaction $ do
    mWorkspaceUuid <- requireSettingsScope
    case mWorkspaceUuid of
      Nothing -> throwError $ UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED
      Just workspaceUuid -> do
        checkSettingsPermission mWorkspaceUuid
        tenantUuid <- asks (.tenantUuid')
        forM_ (section.childTables ++ [section.table]) (\tableName -> deleteSettingsRowsByKey (scopedKey tableName tenantUuid (Just workspaceUuid)))

modifyOverrideAllowed :: WizardRequestContextC s m => OverridableSettings m a -> U.UUID -> Bool -> m ()
modifyOverrideAllowed section tenantUuid overrideAllowed = do
  updateSettingsOverrideAllowed section.table tenantUuid overrideAllowed
  unless overrideAllowed (deleteSettingsWorkspaceRows (section.childTables ++ [section.table]) tenantUuid)

toOverridableSettingsDTO :: WizardRequestContextC s m => OverridableSettings m a -> Maybe U.UUID -> m (SettingsDTO a)
toOverridableSettingsDTO section mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  multiWorkspace <- asks (.tenantMultiWorkspace')
  overrideAllowed <- findSettingsOverrideAllowed section.table tenantUuid >>= requireSettings
  let mOverrideAllowed = if multiWorkspace then Just overrideAllowed else Nothing
  orgValue <- section.findFn tenantUuid Nothing >>= requireSettings
  case mWorkspaceUuid of
    Nothing -> return $ SettingsDTO orgValue mOverrideAllowed Nothing
    Just workspaceUuid -> do
      mOverride <- section.findFn tenantUuid (Just workspaceUuid)
      return $ SettingsDTO (fromMaybe orgValue mOverride) mOverrideAllowed (Just $ isJust mOverride)
