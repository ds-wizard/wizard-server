module Shared.Api.Handler.Project.Event.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.Project.Event.ProjectEventChangeJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.Event.ProjectEventService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectEventChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "events"
    :> Verb POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectEventChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_POST mTokenHeader mServerUrl reqDto uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        addEventToProject uuid reqDto
        return NoContent
