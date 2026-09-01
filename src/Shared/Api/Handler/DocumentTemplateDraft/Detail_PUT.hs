module Shared.Api.Handler.DocumentTemplateDraft.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateDraftChangeDTO
    :> "document-template-drafts"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateDraftDetail)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DocumentTemplateDraftChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateDraftDetail)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyDraft uuid reqDto
