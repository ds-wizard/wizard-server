module Shared.Api.Handler.DocumentTemplateDraft.Asset.Detail_Content_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetService

type Detail_Content_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "documentTemplateUuid" U.UUID
    :> "assets"
    :> Capture "assetUuid" U.UUID
    :> "content"
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

detail_content_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
detail_content_GET mTokenHeader mServerUrl dtUuid assetUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ do
      (asset, result) <- getAssetContent dtUuid assetUuid
      let cdHeader = asset.contentType
      traceUuid <- asks (.traceUuid')
      return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStream $ result
