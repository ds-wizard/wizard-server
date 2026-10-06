module Shared.Api.Handler.DocumentTemplate.List_All_GET where

import Data.Maybe (maybeToList)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Model.Context.TransactionState
import Shared.Service.DocumentTemplate.DocumentTemplateService

type List_All_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> "all"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> QueryParam "id" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [DocumentTemplateSuggestionDTO])

list_all_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe Bool
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] [DocumentTemplateSuggestionDTO])
list_all_GET mTokenHeader mServerUrl mW mTenant mId =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let queryParams = maybeToList ((,) "id" <$> mId)
        getDocumentTemplatesDto queryParams
