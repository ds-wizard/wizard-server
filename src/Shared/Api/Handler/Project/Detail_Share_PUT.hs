module Shared.Api.Handler.Project.Detail_Share_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.ProjectShareChangeDTO
import Shared.Api.Resource.Project.ProjectShareChangeJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.ProjectService

type Detail_Share_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectShareChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "share"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectShareChangeDTO)

detail_share_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectShareChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectShareChangeDTO)
detail_share_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyProjectShare uuid reqDto
