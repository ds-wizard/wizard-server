module Shared.Api.Handler.Project.Detail_Questionnaire_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.ProjectService

type Detail_Questionnaire_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "questionnaire"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDetailQuestionnaireDTO)

detail_questionnaire_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectDetailQuestionnaireDTO)
detail_questionnaire_GET mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getProjectDetailQuestionnaireByUuid uuid
