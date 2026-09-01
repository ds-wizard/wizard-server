module Shared.Api.Handler.KnowledgeModel.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.KnowledgeModel.KnowledgeModelService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelChangeDTO
    :> "knowledge-models"
    :> "preview"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModel)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> KnowledgeModelChangeDTO
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModel)
list_POST mTokenHeader mServerUrl reqDto =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ addTraceUuidHeader =<< createKnowledgeModelPreview reqDto
