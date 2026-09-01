module Shared.Api.Handler.Dev.Operation.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Dev.DevSectionDTO
import Shared.Api.Resource.Dev.DevSectionJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Dev.WizardDevOperationService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "dev-operations"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [DevSectionDTO])

list_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] [DevSectionDTO])
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getDevOperations'
