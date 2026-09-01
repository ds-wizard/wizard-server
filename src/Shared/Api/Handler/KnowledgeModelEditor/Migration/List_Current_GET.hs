module Shared.Api.Handler.KnowledgeModelEditor.Migration.List_Current_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService

type List_Current_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> "migrations"
    :> "current"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelMigrationDTO)

list_current_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelMigrationDTO)
list_current_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getCurrentMigrationDto uuid
