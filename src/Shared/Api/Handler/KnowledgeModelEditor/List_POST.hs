module Shared.Api.Handler.KnowledgeModelEditor.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Service.KnowledgeModel.Editor.EditorService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelEditorCreateDTO
    :> "knowledge-model-editors"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorList)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> KnowledgeModelEditorCreateDTO
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorList)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createEditor reqDto
