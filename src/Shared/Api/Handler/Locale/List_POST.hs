module Shared.Api.Handler.Locale.List_POST where

import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Locale.Common.LocaleUploadDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadJM ()
import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadResultJM ()
import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Locale.LocaleService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem LocaleUploadDTO
    :> "locales"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (LocaleUploadResultDTO LocaleDTO))

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> LocaleUploadDTO
  -> sm (Headers '[Header "x-trace-uuid" String] (LocaleUploadResultDTO LocaleDTO))
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< uploadLocale reqDto
