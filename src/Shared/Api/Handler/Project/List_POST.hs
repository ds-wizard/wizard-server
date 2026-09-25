module Shared.Api.Handler.Project.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.ProjectCreateDTO
import Shared.Api.Resource.Project.ProjectCreateJM ()
import Shared.Api.Resource.Project.ProjectDTO
import Shared.Api.Resource.Project.ProjectJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.ProjectService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectCreateDTO
    :> "projects"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDTO)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectCreateDTO
  -> Maybe U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectDTO)
list_POST mTokenHeader mServerUrl reqDto mW mTenant =
  getScopedMaybeAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ addTraceUuidHeader =<< createProject reqDto
