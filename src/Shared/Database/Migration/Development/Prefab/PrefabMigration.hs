module Shared.Database.Migration.Development.Prefab.PrefabMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Prefab.PrefabDAO
import Shared.Database.Migration.Development.Prefab.Data.Prefabs
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

runMigration :: RequestContextC sc s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Prefab/Prefab) started"
  deletePrefabs
  insertPrefab kmIntegrationBioPortalPrefab
  insertPrefab authServicePrefab
  insertPrefab differentPrefab
  logInfo _CMP_MIGRATION "(Prefab/Prefab) ended"
