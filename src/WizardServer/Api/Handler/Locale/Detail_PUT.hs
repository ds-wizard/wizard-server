module WizardServer.Api.Handler.Locale.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Model.Context.TransactionState
import WizardServer.Api.Resource.Locale.LocaleChangeJM ()
import WizardServer.Api.Resource.Locale.LocaleDTO
import WizardServer.Api.Resource.Locale.LocaleJM ()
import WizardServer.Service.Locale.LocaleService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] LocaleChangeDTO
    :> "locales"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDTO)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> LocaleChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] LocaleDTO)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyLocale uuid reqDto
