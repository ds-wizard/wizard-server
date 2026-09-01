module Shared.Service.KnowledgeModel.Metamodel.MigrationCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.Migration.Metamodel.MigrateToLatestMetamodelVersionCommand
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Service.KnowledgeModel.Metamodel.MigrationService
import Shared.Util.Logger

cComponent = "metamodel_migrator"

execute :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
execute command
  | command.function == cMigrateName = cMigrate command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cMigrateName = "migrate"

cMigrate :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
cMigrate persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String MigrateToLatestMetamodelVersionCommand
  case eCommand of
    Right command -> do
      migrateTenant
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
