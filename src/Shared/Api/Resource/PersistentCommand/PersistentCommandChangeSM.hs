module Shared.Api.Resource.PersistentCommand.PersistentCommandChangeSM where

import Data.Swagger

import Shared.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandChangeJM ()
import Shared.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Util.Swagger

instance ToSchema PersistentCommandChangeDTO where
  declareNamedSchema = toSwagger (PersistentCommandChangeDTO {state = IgnorePersistentCommandState})
