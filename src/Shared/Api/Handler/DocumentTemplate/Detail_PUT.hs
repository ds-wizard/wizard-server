module Shared.Api.Handler.DocumentTemplate.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.DocumentTemplate.DocumentTemplateService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateChangeDTO
    :> "document-templates"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DocumentTemplateChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyDocumentTemplate uuid reqDto
