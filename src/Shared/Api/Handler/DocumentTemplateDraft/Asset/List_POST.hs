module Shared.Api.Handler.DocumentTemplateDraft.Asset.List_POST where

import qualified Data.UUID as U
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateJM ()
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem DocumentTemplateAssetCreateDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateUuid" U.UUID
    :> "assets"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateAssetDTO)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DocumentTemplateAssetCreateDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateAssetDTO)
list_POST mTokenHeader mServerUrl reqDto dtUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        createAsset dtUuid reqDto
