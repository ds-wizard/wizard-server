module Shared.Api.Handler.Project.Detail_Report_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailReportJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Report.ReportService

type Detail_Report_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "report"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDetailReportDTO)

detail_report_GET
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] ProjectDetailReportDTO)
detail_report_GET mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getReportByProjectUuid uuid
