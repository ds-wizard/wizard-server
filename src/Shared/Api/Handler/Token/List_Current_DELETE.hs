module Shared.Api.Handler.Token.List_Current_DELETE where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon hiding (getCurrentUser)
import Shared.Model.Context.TransactionState
import Shared.Service.UserToken.Login.LoginService

type List_Current_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "tokens"
    :> "current"
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_DELETE
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_DELETE mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteLoginTokenByValue mTokenHeader
        return NoContent
