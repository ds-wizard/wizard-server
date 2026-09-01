module WizardServer.Api.Handler.OpenIdClient.Detail_Logout_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.UserToken.Login.LoginService

type Detail_Logout_GET =
  Header "Host" String
    :> "open-id-clients"
    :> Capture "uuid" U.UUID
    :> "logout"
    :> QueryParam "sid" String
    :> Verb GET 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_logout_GET
  :: WizardHandlerC s sm r rm => Maybe String -> U.UUID -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_logout_GET mServerUrl _providerUuid mSid =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< do
      deleteLoginTokenBySessionState mSid
      return NoContent
