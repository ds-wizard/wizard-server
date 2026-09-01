module Shared.Api.Handler.Project.Version.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Shared.Api.Resource.Project.Version.ProjectVersionChangeJM ()
import Shared.Api.Resource.Project.Version.ProjectVersionListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Service.Project.Version.ProjectVersionService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectVersionChangeDTO
    :> "projects"
    :> Capture "projectUuid" U.UUID
    :> "versions"
    :> Capture "versionUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectVersionList)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectVersionChangeDTO
  -> U.UUID
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectVersionList)
detail_PUT mTokenHeader mServerUrl reqDto projectUuid versionUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyVersion projectUuid versionUuid reqDto
