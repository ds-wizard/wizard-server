module WizardServer.Api.Handler.Locale.List_POST where

import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Locale.LocaleCreateDTO
import Shared.Api.Resource.Locale.LocaleSimpleJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Locale.LocaleSimple
import WizardServer.Api.Resource.Locale.LocaleCreateJM ()
import WizardServer.Service.Locale.LocaleService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem LocaleCreateDTO
    :> "locales"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleSimple)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> LocaleCreateDTO
  -> sm (Headers '[Header "x-trace-uuid" String] LocaleSimple)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader
        =<< createLocale reqDto
