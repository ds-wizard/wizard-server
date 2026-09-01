module Shared.Api.Handler.Project.List_POST_FromTemplate where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.ProjectCreateFromTemplateDTO
import Shared.Api.Resource.Project.ProjectCreateFromTemplateJM ()
import Shared.Api.Resource.Project.ProjectDTO
import Shared.Api.Resource.Project.ProjectJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.ProjectService

type List_POST_FromTemplate =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectCreateFromTemplateDTO
    :> "projects"
    :> "from-template"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDTO)

list_POST_FromTemplate
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectCreateFromTemplateDTO
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectDTO)
list_POST_FromTemplate mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createProjectFromTemplate reqDto
