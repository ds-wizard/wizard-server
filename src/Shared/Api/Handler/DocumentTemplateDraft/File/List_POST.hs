module Shared.Api.Handler.DocumentTemplateDraft.File.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateFileChangeDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateUuid" U.UUID
    :> "files"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DocumentTemplateFileChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)
list_POST mTokenHeader mServerUrl reqDto dtUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createFile dtUuid reqDto
