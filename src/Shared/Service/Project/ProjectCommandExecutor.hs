module Shared.Service.Project.ProjectCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.Project.CreateProjectCommand
import Shared.Service.Project.ProjectService
import Shared.Util.Logger

cComponent = "project"

execute :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateProjectsName = cCreateProjects command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreateProjectsName = "createProjects"

cCreateProjects :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
cCreateProjects persistentCommand = do
  let eCommands = eitherDecode (BSL.pack persistentCommand.body) :: Either String [CreateProjectCommand]
  case eCommands of
    Right commands -> do
      createProjectsFromCommands commands
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
