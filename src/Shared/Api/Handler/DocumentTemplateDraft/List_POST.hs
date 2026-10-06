module Shared.Api.Handler.DocumentTemplateDraft.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateDraftCreateDTO
    :> "document-template-drafts"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DocumentTemplateDraftCreateDTO
  -> Maybe U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)
list_POST mTokenHeader mServerUrl reqDto mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createDraft reqDto
