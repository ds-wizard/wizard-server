module Shared.Api.Handler.Submission.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Submission.SubmissionCreateDTO
import Shared.Api.Resource.Submission.SubmissionCreateJM ()
import Shared.Api.Resource.Submission.SubmissionJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Submission.SubmissionList
import Shared.Service.Submission.SubmissionService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] SubmissionCreateDTO
    :> "documents"
    :> Capture "docUuid" U.UUID
    :> "submissions"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] SubmissionList)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> SubmissionCreateDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] SubmissionList)
list_POST mTokenHeader mServerUrl reqDto docUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< submitDocument docUuid reqDto
