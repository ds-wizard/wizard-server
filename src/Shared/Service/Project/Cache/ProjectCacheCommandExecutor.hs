module Shared.Service.Project.Cache.ProjectCacheCommandExecutor where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.Project.RefreshProjectCacheCommand
import Shared.Service.PersistentCommand.PersistentCommandMapper
import Shared.Service.Project.Cache.ProjectCacheService
import Shared.Util.Logger
import Shared.Util.Uuid

cComponent = "project_cache"

execute :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
execute command
  | command.function == cRefreshName = cRefresh command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cRefreshName = "refresh"

cRefresh :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
cRefresh persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String RefreshProjectCacheCommand
  case eCommand of
    Right _ -> do
      failures <- refreshProjectCaches
      if null failures
        then do
          uuid <- liftIO generateUuid
          now <- liftIO getCurrentTime
          void . insertPersistentCommand $ toPersistentCommand uuid "analytics" "synchronize" persistentCommand.body 10 persistentCommand.tenantUuid persistentCommand.createdBy now
          return (DonePersistentCommandState, Nothing)
        else return (ErrorPersistentCommandState, Just . L.intercalate "\n" $ failures)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
