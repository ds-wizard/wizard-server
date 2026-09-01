module Shared.Api.Handler.Project.Detail_Preview_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Detail.ProjectDetailPreviewJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Project.Detail.ProjectDetailPreview
import Shared.Service.Project.ProjectService

type Detail_Preview_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "preview"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDetailPreview)

detail_preview_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectDetailPreview)
detail_preview_GET mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getProjectDetailPreviewById uuid
