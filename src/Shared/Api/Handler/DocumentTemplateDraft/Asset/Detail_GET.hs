module Shared.Api.Handler.DocumentTemplateDraft.Asset.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "documentTemplateUuid" U.UUID
    :> "assets"
    :> Capture "assetUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateAssetDTO)

detail_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateAssetDTO)
detail_GET mTokenHeader mServerUrl tmlId assetUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getAsset assetUuid
