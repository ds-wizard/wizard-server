module WizardServer.Api.Handler.User.Tour.Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon hiding (getCurrentUser)
import Shared.Api.Resource.User.UserDTO
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.TransactionState
import Shared.Service.User.Tour.TourService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "tours"
    :> Capture "tourId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_PUT :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> String -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_PUT mTokenHeader mServerUrl tourId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        user <- getCurrentUser
        createOrUpdateTour user.uuid tourId
        return NoContent
