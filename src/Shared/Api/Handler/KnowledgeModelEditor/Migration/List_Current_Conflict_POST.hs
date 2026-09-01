module Shared.Api.Handler.KnowledgeModelEditor.Migration.List_Current_Conflict_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService

type List_Current_Conflict_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelMigrationResolutionDTO
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> "migrations"
    :> "current"
    :> "conflict"
    :> Verb 'POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_conflict_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> KnowledgeModelMigrationResolutionDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_conflict_POST mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        solveConflictAndMigrate uuid reqDto
        return NoContent
