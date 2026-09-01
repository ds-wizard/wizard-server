module Shared.Api.Handler.KnowledgeModelEditor.Migration.List_Current_Conflict_All_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService

type List_Current_Conflict_All_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> "migrations"
    :> "current"
    :> "conflict"
    :> "all"
    :> Verb 'POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_Current_Conflict_All_POST
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_Current_Conflict_All_POST mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        solveAllConflicts uuid
        return NoContent
