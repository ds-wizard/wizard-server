module Shared.Api.Handler.KnowledgeModelSecret.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-secrets"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [KnowledgeModelSecret])

list_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Maybe U.UUID -> Maybe Bool -> sm (Headers '[Header "x-trace-uuid" String] [KnowledgeModelSecret])
list_GET mTokenHeader mServerUrl mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getKnowledgeModelSecrets
