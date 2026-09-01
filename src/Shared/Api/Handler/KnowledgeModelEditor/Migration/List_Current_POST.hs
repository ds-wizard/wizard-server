module Shared.Api.Handler.KnowledgeModelEditor.Migration.List_Current_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateJM ()
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService

type List_Current_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelMigrationCreateDTO
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> "migrations"
    :> "current"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelMigrationDTO)

list_current_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> KnowledgeModelMigrationCreateDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelMigrationDTO)
list_current_POST mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createMigration uuid reqDto
