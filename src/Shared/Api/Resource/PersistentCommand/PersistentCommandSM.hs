module Shared.Api.Resource.PersistentCommand.PersistentCommandSM where

import Data.Swagger

import Shared.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Shared.Model.PersistentCommand.PersistentCommand

instance ToSchema PersistentCommandState
