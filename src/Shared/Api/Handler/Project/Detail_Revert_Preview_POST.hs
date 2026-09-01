module Shared.Api.Handler.Project.Detail_Revert_Preview_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.ProjectContentDTO
import Shared.Api.Resource.Project.ProjectContentJM ()
import Shared.Api.Resource.Project.Version.ProjectVersionRevertDTO
import Shared.Api.Resource.Project.Version.ProjectVersionRevertJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.Version.ProjectVersionService

type Detail_Revert_Preview_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectVersionRevertDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "revert"
    :> "preview"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectContentDTO)

detail_revert_preview_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectVersionRevertDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectContentDTO)
detail_revert_preview_POST mTokenHeader mServerUrl reqDto uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< revertToEvent uuid reqDto False
