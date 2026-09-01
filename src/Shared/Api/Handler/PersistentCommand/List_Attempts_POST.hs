module Shared.Api.Handler.PersistentCommand.List_Attempts_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.PersistentCommand.WizardPersistentCommandService

type List_Attempts_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "persistent-commands"
    :> "attempts"
    :> Verb 'POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_attempts_POST :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_attempts_POST mTokenHeader mServerUrl =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        runPersistentCommands'
        return NoContent
