module Shared.Api.Handler.Project.Detail_Settings_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Detail.ProjectDetailSettingsJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Project.Detail.ProjectDetailSettings
import Shared.Service.Project.ProjectService

type Detail_Settings_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "settings"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDetailSettings)

detail_settings_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectDetailSettings)
detail_settings_GET mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getProjectDetailSettingsById uuid
