module RegistryPublic.Api.Handler.PersistentCommand.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Shared.Model.PersistentCommand.PersistentCommand

type List_POST =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] (PersistentCommand String)
    :> "persistent-commands"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (PersistentCommand String))
