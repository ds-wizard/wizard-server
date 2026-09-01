module Shared.Api.Handler.DocumentTemplateDraft.Detail_Documents_Preview_Settings_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftService

type Detail_Documents_Preview_Settings_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateDraftDataChangeDTO
    :> "document-template-drafts"
    :> Capture "uuid" U.UUID
    :> "documents"
    :> "preview"
    :> "settings"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateDraftDataDTO)

detail_documents_preview_settings_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DocumentTemplateDraftDataChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateDraftDataDTO)
detail_documents_preview_settings_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyDraftData uuid reqDto
