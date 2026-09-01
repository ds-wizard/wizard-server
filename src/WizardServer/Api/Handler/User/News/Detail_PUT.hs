module WizardServer.Api.Handler.User.News.Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon hiding (getCurrentUser)
import Shared.Api.Resource.User.UserDTO
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.TransactionState
import WizardServer.Service.User.News.NewsService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "news"
    :> Capture "lastSeenNewsId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_PUT :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> String -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_PUT mTokenHeader mServerUrl lastSeenNewsId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        user <- getCurrentUser
        updateNews user.uuid lastSeenNewsId
        return NoContent
