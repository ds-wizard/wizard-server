module Shared.Api.Handler.DocumentTemplateDraft.Asset.Detail_Content_PUT where

import qualified Data.UUID as U
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetService

type Detail_Content_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem DocumentTemplateAssetCreateDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateUuid" U.UUID
    :> "assets"
    :> Capture "assetUuid" U.UUID
    :> "content"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateAsset)

detail_content_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DocumentTemplateAssetCreateDTO
  -> U.UUID
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateAsset)
detail_content_PUT mTokenHeader mServerUrl reqDto dtUuid assetUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        modifyAssetContent assetUuid reqDto
