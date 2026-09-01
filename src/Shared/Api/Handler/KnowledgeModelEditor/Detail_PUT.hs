module Shared.Api.Handler.KnowledgeModelEditor.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Editor.EditorService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelEditorChangeDTO
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorDetailDTO)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> KnowledgeModelEditorChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorDetailDTO)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyEditor uuid reqDto
