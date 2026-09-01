module Shared.Api.Resource.PersistentCommand.WizardPersistentCommandSM where

import Data.Swagger
import qualified Data.UUID as U

import Shared.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.Database.Migration.Development.PersistentCommand.Data.WizardPersistentCommands
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Util.Swagger

instance ToSchema (PersistentCommand U.UUID) where
  declareNamedSchema = toSwagger command1
