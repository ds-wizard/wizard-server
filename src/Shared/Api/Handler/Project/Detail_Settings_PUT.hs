module Shared.Api.Handler.Project.Detail_Settings_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.ProjectSettingsChangeDTO
import Shared.Api.Resource.Project.ProjectSettingsChangeJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.ProjectService

type Detail_Settings_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectSettingsChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "settings"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectSettingsChangeDTO)

detail_settings_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectSettingsChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectSettingsChangeDTO)
detail_settings_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyProjectSettings uuid reqDto
