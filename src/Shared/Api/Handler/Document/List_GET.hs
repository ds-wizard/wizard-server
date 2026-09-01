module Shared.Api.Handler.Document.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.Document.DocumentJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Service.Document.DocumentService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "documents"
    :> QueryParam "projectUuid" U.UUID
    :> QueryParam "documentTemplateUuid" U.UUID
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page DocumentDTO))

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe U.UUID
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page DocumentDTO))
list_GET mTokenHeader mServerUrl mProjectUuid mDocumentTemplateUuid mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getDocumentsPageDto mProjectUuid mDocumentTemplateUuid mQuery (Pageable mPage mSize) (parseSortQuery mSort)
