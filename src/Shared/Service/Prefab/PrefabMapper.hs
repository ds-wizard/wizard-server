module Shared.Service.Prefab.PrefabMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Model.PersistentCommand.Prefab.CreateOrUpdatePrefabCommand
import Shared.Model.Prefab.Prefab

fromCommandCreate :: Prefab -> CreateOrUpdatePrefabCommand -> U.UUID -> UTCTime -> Prefab
fromCommandCreate prefab command tenantUuid now =
  Prefab
    { uuid = command.uuid
    , pType = command.pType
    , name = command.name
    , content = command.content
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromCommandChange :: Prefab -> CreateOrUpdatePrefabCommand -> UTCTime -> Prefab
fromCommandChange prefab command now =
  Prefab
    { uuid = prefab.uuid
    , pType = command.pType
    , name = command.name
    , content = command.content
    , tenantUuid = prefab.tenantUuid
    , createdAt = prefab.createdAt
    , updatedAt = now
    }
