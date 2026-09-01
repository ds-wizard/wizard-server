module WizardServer.Api.Handler.Locale.List_Bundle_POST where

import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Common.FileDTO
import Shared.Api.Resource.Common.FileJM ()
import Shared.Api.Resource.Locale.LocaleSimpleJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Locale.LocaleSimple
import Shared.Service.Locale.Bundle.LocaleBundleService

type List_Bundle_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem FileDTO
    :> "locales"
    :> "bundle"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleSimple)

list_bundle_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> FileDTO
  -> sm (Headers '[Header "x-trace-uuid" String] LocaleSimple)
list_bundle_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        importBundle reqDto.content False
