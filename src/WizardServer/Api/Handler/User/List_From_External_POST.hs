module WizardServer.Api.Handler.User.List_From_External_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.UserFromExternalDTO
import Shared.Api.Resource.User.UserFromExternalJM ()
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Model.Context.TransactionState
import Shared.Service.User.RegistrationPending.UserRegistrationPendingService

type List_From_External_POST =
  Header "Host" String
    :> Header "Accept-Language" String
    :> Header "User-Agent" String
    :> ReqBody '[SafeJSON] UserFromExternalDTO
    :> "users"
    :> "from-external"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_from_external_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> UserFromExternalDTO
  -> sm (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_from_external_POST mServerUrl mAcceptLanguages mUserAgent reqDto =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< completeExternalRegistration reqDto mAcceptLanguages mUserAgent
