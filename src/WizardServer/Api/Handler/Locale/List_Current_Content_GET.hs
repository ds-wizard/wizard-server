module WizardServer.Api.Handler.Locale.List_Current_Content_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import WizardServer.Service.Locale.LocaleService

type List_Current_Content_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> "current"
    :> "content"
    :> QueryParam "clientUrl" String
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

list_current_content_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
list_current_content_GET mTokenHeader mServerUrl mClientUrl =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ do
      locale <- getLocaleContentForCurrentUser mClientUrl
      traceUuid <- asks (.traceUuid')
      return . addHeader (U.toString traceUuid) . addHeader "application/json" . FileStream $ locale
