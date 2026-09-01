module WizardServer.Api.Handler.User.Tour.List_DELETE where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon hiding (getCurrentUser)
import Shared.Api.Resource.User.UserDTO
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.TransactionState
import WizardServer.Service.User.Tour.TourService

type List_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "tours"
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_DELETE :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_DELETE mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        user <- getCurrentUser
        deleteTours user.uuid
        return NoContent
