module Shared.Api.Handler.Project.Version.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Version.ProjectVersionListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Service.Project.Version.ProjectVersionService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "versions"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [ProjectVersionList])

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] [ProjectVersionList])
list_GET mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getVersions uuid
