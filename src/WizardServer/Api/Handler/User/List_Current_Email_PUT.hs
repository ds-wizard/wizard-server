module WizardServer.Api.Handler.User.List_Current_Email_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.User.UserService

type List_Current_Email_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "email"
    :> QueryParam' '[Required] "hash" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_email_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> String
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_email_PUT mTokenHeader mServerUrl hash =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        confirmEmailChange hash
        return NoContent
