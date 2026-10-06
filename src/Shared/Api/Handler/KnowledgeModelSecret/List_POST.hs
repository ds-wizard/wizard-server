module Shared.Api.Handler.KnowledgeModelSecret.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeJM ()
import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelSecretChangeDTO
    :> "knowledge-model-secrets"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelSecret)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> KnowledgeModelSecretChangeDTO
  -> Maybe U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelSecret)
list_POST mTokenHeader mServerUrl reqDto mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createKnowledgeModelSecret reqDto
