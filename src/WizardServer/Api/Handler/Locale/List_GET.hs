module WizardServer.Api.Handler.Locale.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import WizardServer.Api.Resource.Locale.LocaleDTO
import WizardServer.Api.Resource.Locale.LocaleJM ()
import WizardServer.Service.Locale.LocaleService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> QueryParam "organizationId" String
    :> QueryParam "localeId" String
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page LocaleDTO))

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page LocaleDTO))
list_GET mTokenHeader mServerUrl mOrganizationId mLocaleId mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getLocalesPage mOrganizationId mLocaleId mQuery (Pageable mPage mSize) (parseSortQuery mSort)
