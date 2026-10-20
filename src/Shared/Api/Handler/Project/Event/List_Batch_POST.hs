module Shared.Api.Handler.Project.Event.List_Batch_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Event.ProjectEventDTO
import Shared.Api.Resource.Project.Event.ProjectEventJM ()
import Shared.Api.Resource.Project.ProjectContentChangeDTO
import Shared.Api.Resource.Project.ProjectContentChangeJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.Event.ProjectEventService

type List_Batch_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectContentChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "events"
    :> "batch"
    :> Verb POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [ProjectEventDTO])

list_batch_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectContentChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] [ProjectEventDTO])
list_batch_POST mTokenHeader mServerUrl reqDto uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< addEventsToProject uuid reqDto
