module Shared.Api.Handler.Project.Version.List_POST where

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

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectVersionChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "versions"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectVersionList)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectVersionChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectVersionList)
list_POST mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createVersion uuid reqDto
