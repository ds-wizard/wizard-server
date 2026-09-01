module Shared.Api.Handler.Project.Detail_Content_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.ProjectContentChangeDTO
import Shared.Api.Resource.Project.ProjectContentChangeJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.ProjectService

type Detail_Content_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectContentChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "content"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectContentChangeDTO)

detail_content_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectContentChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectContentChangeDTO)
detail_content_PUT mTokenHeader mServerUrl reqDto uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyContent uuid reqDto
