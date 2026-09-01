module Shared.Api.Handler.Project.Migration.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireJM ()
import Shared.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Shared.Api.Resource.Project.Migration.ProjectMigrationCreateJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.Migration.ProjectMigrationService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectMigrationCreateDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "migrations"
    :> Verb 'POST 200 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDetailQuestionnaireDTO)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectMigrationCreateDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectDetailQuestionnaireDTO)
list_POST mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< migrateProject uuid reqDto
