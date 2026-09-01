module Shared.Api.Handler.DocumentTemplateDraft.Asset.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateAssetChangeDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateUuid" U.UUID
    :> "assets"
    :> Capture "assetUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateAsset)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DocumentTemplateAssetChangeDTO
  -> U.UUID
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateAsset)
detail_PUT mTokenHeader mServerUrl reqDto dtUuid assetUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyAsset assetUuid reqDto
