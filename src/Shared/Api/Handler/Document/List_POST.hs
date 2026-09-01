module Shared.Api.Handler.Document.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Document.DocumentCreateDTO
import Shared.Api.Resource.Document.DocumentCreateJM ()
import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.Document.DocumentJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Document.DocumentService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentCreateDTO
    :> "documents"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentDTO)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DocumentCreateDTO
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createDocument reqDto
