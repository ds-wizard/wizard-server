module Shared.Api.Handler.DocumentTemplateDraft.Folder.List_Delete_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteDTO
import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.DocumentTemplate.Folder.DocumentTemplateFolderService

type List_Delete_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateFolderDeleteDTO
    :> "document-template-drafts"
    :> Capture "uuid" U.UUID
    :> "folders"
    :> "delete"
    :> Verb POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_delete_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DocumentTemplateFolderDeleteDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_delete_POST mTokenHeader mServerUrl reqDto documentTemplateUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteDraftFolder documentTemplateUuid reqDto
        return NoContent
