module Shared.Service.Prefab.PrefabCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Shared.Model.Context.RequestContext
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.Prefab.CreateOrUpdatePrefabCommand
import Shared.Model.PersistentCommand.Prefab.DeletePrefabCommand
import Shared.Service.Prefab.PrefabService
import Shared.Util.Logger

cComponent = "prefab"

execute :: RequestContextC s sc m => PersistentCommand identity -> m (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreatePrefabName = cCreatePrefab command
  | command.function == cUpdatePrefabName = cUpdatePrefab command
  | command.function == cDeletePrefabName = cDeletePrefab command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreatePrefabName = "createPrefab"

cCreatePrefab :: RequestContextC s sc m => PersistentCommand identity -> m (PersistentCommandState, Maybe String)
cCreatePrefab persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdatePrefabCommand
  case eCommand of
    Right command -> do
      createPrefab command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdatePrefabName = "updatePrefab"

cUpdatePrefab :: RequestContextC s sc m => PersistentCommand identity -> m (PersistentCommandState, Maybe String)
cUpdatePrefab persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdatePrefabCommand
  case eCommand of
    Right command -> do
      modifyPrefab command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cDeletePrefabName = "deletePrefab"

cDeletePrefab :: RequestContextC s sc m => PersistentCommand identity -> m (PersistentCommandState, Maybe String)
cDeletePrefab persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String DeletePrefabCommand
  case eCommand of
    Right command -> do
      deletePrefab command.uuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
