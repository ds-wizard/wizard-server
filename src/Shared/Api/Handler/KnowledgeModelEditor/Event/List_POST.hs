module Shared.Api.Handler.KnowledgeModelEditor.Event.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelEditorEventsChangeDTO
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> "events"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorEventsDTO)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> KnowledgeModelEditorEventsChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorEventsDTO)
list_POST mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createEditorEvents uuid reqDto
