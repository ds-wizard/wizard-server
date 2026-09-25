module Shared.Api.Handler.DocumentTemplate.List_DELETE where

import Data.Maybe (catMaybes)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.DocumentTemplate.DocumentTemplateService

type List_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> QueryParam "organizationId" String
    :> QueryParam "templateId" String
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_DELETE
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_DELETE mTokenHeader mServerUrl mOrganizationId mTemplateId mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        let queryParams = catMaybes [(,) "organization_id" <$> mOrganizationId, (,) "template_id" <$> mTemplateId]
        deleteDocumentTemplatesByQueryParams queryParams
        return NoContent
