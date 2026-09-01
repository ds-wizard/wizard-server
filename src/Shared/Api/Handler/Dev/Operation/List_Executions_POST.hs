module Shared.Api.Handler.Dev.Operation.List_Executions_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Dev.DevExecutionDTO
import Shared.Api.Resource.Dev.DevExecutionJM ()
import Shared.Api.Resource.Dev.DevExecutionResultDTO
import Shared.Api.Resource.Dev.DevExecutionResultJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Dev.WizardDevOperationService

type List_Executions_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DevExecutionDTO
    :> "dev-operations"
    :> "executions"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] AdminExecutionResultDTO)

list_executions_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DevExecutionDTO
  -> sm (Headers '[Header "x-trace-uuid" String] AdminExecutionResultDTO)
list_executions_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< executeOperation' reqDto
