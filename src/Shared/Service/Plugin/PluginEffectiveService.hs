module Shared.Service.Plugin.PluginEffectiveService where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Database.DAO.Plugin.PluginDAO
import Shared.Database.DAO.Plugin.TenantPluginSettingsDAO
import Shared.Database.DAO.Plugin.WorkspacePluginSettingsDAO
import Shared.Localization.Messages.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Plugin.EffectivePlugin
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.TenantPluginSettings
import Shared.Model.Plugin.WorkspacePluginSettings

getEffectivePlugins :: WizardRequestContextC s m => U.UUID -> m [EffectivePlugin]
getEffectivePlugins workspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  plugins <- filter (.enabled) <$> findPluginsByTenantUuid tenantUuid
  orgValues <- findTenantPluginSettingValues tenantUuid
  rows <- inMultiWorkspace [] (findWorkspacePluginSettingsByWorkspaceUuid workspaceUuid)
  let rowsByPlugin = M.fromList [(row.pluginUuid, row) | row <- rows]
  return [toEffectivePlugin plugin (M.lookup plugin.uuid orgValues) (M.lookup plugin.uuid rowsByPlugin) | plugin <- plugins]

getEffectivePlugin :: WizardRequestContextC s m => U.UUID -> U.UUID -> m EffectivePlugin
getEffectivePlugin workspaceUuid pluginUuid = do
  plugin <- getOrganizationEnabledPlugin pluginUuid
  mOrgValues <- fmap (.values) <$> findTenantPluginSettingsByPluginUuid' pluginUuid
  mRow <- inMultiWorkspace Nothing (findWorkspacePluginSettings' workspaceUuid pluginUuid)
  return $ toEffectivePlugin plugin mOrgValues mRow

getOrganizationEnabledPlugin :: WizardRequestContextC s m => U.UUID -> m Plugin
getOrganizationEnabledPlugin pluginUuid = do
  plugin <- findPluginByUuid pluginUuid
  unless plugin.enabled (throwError . NotExistsError $ _ERROR_VALIDATION__ABSENCE "plugin")
  return plugin

inMultiWorkspace :: WizardRequestContextC s m => a -> m a -> m a
inMultiWorkspace none action = do
  multiWorkspace <- asks (.tenantMultiWorkspace')
  if multiWorkspace then action else return none

toEffectivePlugin :: Plugin -> Maybe A.Value -> Maybe WorkspacePluginSettings -> EffectivePlugin
toEffectivePlugin plugin mOrgValues mRow =
  EffectivePlugin
    { plugin = plugin
    , enabled = maybe True (.enabled) mRow
    , overridden = isJust mOverride
    , values = mOverride <|> mOrgValues
    }
  where
    mOverride = mRow >>= (.values)
