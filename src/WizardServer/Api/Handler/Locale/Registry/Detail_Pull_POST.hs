module WizardServer.Api.Handler.Locale.Registry.Detail_Pull_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadResultJM ()
import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleJM ()
import Shared.Api.Resource.Locale.Registry.RegistryLocalePullDTO
import Shared.Api.Resource.Locale.Registry.RegistryLocalePullJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Locale.Registry.RegistryLocaleService

type Detail_Pull_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] RegistryLocalePullDTO
    :> "locales"
    :> "registry"
    :> Capture "code" String
    :> "pull"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (LocaleUploadResultDTO LocaleDTO))

detail_pull_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> RegistryLocalePullDTO
  -> String
  -> sm (Headers '[Header "x-trace-uuid" String] (LocaleUploadResultDTO LocaleDTO))
detail_pull_POST mTokenHeader mServerUrl reqDto code =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< pullRegistryLocale code reqDto
