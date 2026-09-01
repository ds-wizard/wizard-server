module WizardServer.Api.Handler.User.List_Consents_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Auth.AuthConsentDTO
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Model.Context.TransactionState
import Shared.Service.User.UserService

type List_Consents_POST =
  Header "Host" String
    :> Header "User-Agent" String
    :> ReqBody '[SafeJSON] AuthConsentDTO
    :> "users"
    :> "consents"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_consents_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> AuthConsentDTO
  -> sm (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_consents_POST mServerUrl mUserAgent reqDto =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< confirmConsents reqDto Nothing
