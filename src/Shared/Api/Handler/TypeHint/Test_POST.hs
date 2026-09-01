module Shared.Api.Handler.TypeHint.Test_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.TypeHint.TypeHintIJM ()
import Shared.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Shared.Api.Resource.TypeHint.TypeHintTestRequestJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.TypeHint.TypeHintService

type Test_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TypeHintTestRequestDTO
    :> "type-hints"
    :> "test"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TypeHintExchange)

test_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> TypeHintTestRequestDTO
  -> sm (Headers '[Header "x-trace-uuid" String] TypeHintExchange)
test_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< testTypeHints reqDto
