module WizardServer.Api.Handler.User.List_Current_Identities_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.UserOpenIdIdentityDTO
import Shared.Api.Resource.User.UserOpenIdIdentityJM ()
import Shared.Model.Context.TransactionState
import WizardServer.Service.User.ExternalIdentity.UserExternalIdentityService

type List_Current_Identities_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "identities"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [UserOpenIdIdentityDTO])

list_current_identities_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] [UserOpenIdIdentityDTO])
list_current_identities_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getUserIdentities
