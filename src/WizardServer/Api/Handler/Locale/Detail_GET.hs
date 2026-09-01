module WizardServer.Api.Handler.Locale.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import WizardServer.Api.Resource.Locale.LocaleDetailDTO
import WizardServer.Api.Resource.Locale.LocaleDetailJM ()
import WizardServer.Service.Locale.LocaleService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> Capture "uuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDetailDTO)

detail_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] LocaleDetailDTO)
detail_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ addTraceUuidHeader =<< getLocaleByUuid uuid
