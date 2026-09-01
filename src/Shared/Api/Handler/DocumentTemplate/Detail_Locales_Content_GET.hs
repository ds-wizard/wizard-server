module Shared.Api.Handler.DocumentTemplate.Detail_Locales_Content_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleService

type Detail_Locales_Content_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> Capture "uuid" U.UUID
    :> "locales"
    :> Capture "localeUuid" U.UUID
    :> "content"
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

detail_locales_content_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
detail_locales_content_GET mTokenHeader mServerUrl dtUuid localeUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ do
      content <- getLocaleContent dtUuid localeUuid
      traceUuid <- asks (.traceUuid')
      return . addHeader (U.toString traceUuid) . addHeader "application/octet-stream" . FileStream $ content
