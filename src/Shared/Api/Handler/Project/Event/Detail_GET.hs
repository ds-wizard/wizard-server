module Shared.Api.Handler.Project.Event.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Event.ProjectEventDTO
import Shared.Api.Resource.Project.Event.ProjectEventJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.ProjectService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "projectUuid" U.UUID
    :> "events"
    :> Capture "eventUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectEventDTO)

detail_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectEventDTO)
detail_GET mTokenHeader mServerUrl projectUuid eventUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getProjectEventForProjectUuid projectUuid eventUuid
