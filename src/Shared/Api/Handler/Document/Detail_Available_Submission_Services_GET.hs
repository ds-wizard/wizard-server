module Shared.Api.Handler.Document.Detail_Available_Submission_Services_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Tenant.Config.TenantConfigSubmissionServiceSimpleJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import Shared.Service.Submission.SubmissionService

type Detail_Available_Submission_Services_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "documents"
    :> Capture "docUuid" U.UUID
    :> "available-submission-services"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [TenantConfigSubmissionServiceSimple])

detail_available_submission_Services_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] [TenantConfigSubmissionServiceSimple])
detail_available_submission_Services_GET mTokenHeader mServerUrl docUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getAvailableServicesForSubmission docUuid
