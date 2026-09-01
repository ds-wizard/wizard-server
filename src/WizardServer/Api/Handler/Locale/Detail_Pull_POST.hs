module WizardServer.Api.Handler.Locale.Detail_Pull_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Api.Resource.Locale.LocaleSimpleJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Locale.LocaleSimple
import Shared.Service.Locale.Bundle.LocaleBundleService

type Detail_Pull_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> Capture "id" Coordinate
    :> "pull"
    :> Verb POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleSimple)

detail_pull_POST :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Coordinate -> sm (Headers '[Header "x-trace-uuid" String] LocaleSimple)
detail_pull_POST mTokenHeader mServerUrl coordinate =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< pullBundleFromRegistry coordinate
