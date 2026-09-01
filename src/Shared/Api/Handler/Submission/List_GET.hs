module Shared.Api.Handler.Submission.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Submission.SubmissionJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Submission.SubmissionList
import Shared.Service.Submission.SubmissionService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "documents"
    :> Capture "docUuid" U.UUID
    :> "submissions"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [SubmissionList])

list_GET
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] [SubmissionList])
list_GET mTokenHeader mServerUrl docUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getSubmissionsForDocument docUuid
