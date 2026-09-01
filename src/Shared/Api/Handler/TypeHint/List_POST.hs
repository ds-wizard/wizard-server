module Shared.Api.Handler.TypeHint.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.TypeHint.TypeHintIJM ()
import Shared.Api.Resource.TypeHint.TypeHintRequestDTO
import Shared.Api.Resource.TypeHint.TypeHintRequestJM ()
import Shared.Integration.Resource.TypeHint.TypeHintIDTO
import Shared.Model.Context.TransactionState
import Shared.Service.TypeHint.TypeHintService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TypeHintRequestDTO
    :> "type-hints"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [TypeHintIDTO])

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> TypeHintRequestDTO
  -> sm (Headers '[Header "x-trace-uuid" String] [TypeHintIDTO])
list_POST mTokenHeader mServerUrl reqDto@(ProjectTypeHintRequest' _) =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getTypeHints reqDto
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getTypeHints reqDto
