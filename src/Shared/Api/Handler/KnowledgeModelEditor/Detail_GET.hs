module Shared.Api.Handler.KnowledgeModelEditor.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Editor.EditorService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorDetailDTO)

detail_GET
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorDetailDTO)
detail_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getEditorByUuid uuid
