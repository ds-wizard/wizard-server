module Shared.Service.Prefab.PrefabService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U
import GHC.Int

import Shared.Database.DAO.Prefab.PrefabDAO
import Shared.Model.Context.RequestContext
import Shared.Model.PersistentCommand.Prefab.CreateOrUpdatePrefabCommand
import Shared.Model.Prefab.Prefab
import Shared.Service.Prefab.PrefabMapper
import Shared.Util.Uuid

getPrefabsFiltered :: RequestContextC s sc m => [(String, String)] -> m [Prefab]
getPrefabsFiltered = findPrefabsFiltered

createPrefab :: RequestContextC s sc m => CreateOrUpdatePrefabCommand -> m Int64
createPrefab command = do
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let prefab = fromCommandCreate prefab command tenantUuid now
  insertPrefab prefab

modifyPrefab :: RequestContextC s sc m => CreateOrUpdatePrefabCommand -> m Int64
modifyPrefab command = do
  uuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  prefab <- findPrefabByUuid command.uuid
  let updatedPrefab = fromCommandChange prefab command now
  updatePrefabByUuid updatedPrefab

deletePrefab :: RequestContextC s sc m => U.UUID -> m Int64
deletePrefab = deletePrefabByUuid
