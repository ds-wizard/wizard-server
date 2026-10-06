module Shared.Service.Plugin.PluginService where

import Control.Monad (void)
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import Shared.Database.DAO.Plugin.PluginDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Tenant
import Shared.Service.Plugin.PluginMapper

createPluginForAllTenants :: WizardRequestContextC s m => U.UUID -> String -> Bool -> m ()
createPluginForAllTenants uuid url enabled =
  runInTransaction $ do
    tenants <- findTenants
    now <- liftIO getCurrentTime
    traverse_
      ( \tenant -> do
          let plugin = toPlugin uuid url enabled tenant.uuid now
          void $ insertPlugin plugin
      )
      tenants

createPluginForTenant :: WizardRequestContextC s m => U.UUID -> U.UUID -> String -> Bool -> m ()
createPluginForTenant tenantUuid uuid url enabled =
  runInTransaction $ do
    now <- liftIO getCurrentTime
    let plugin = toPlugin uuid url enabled tenantUuid now
    void $ insertPlugin plugin
