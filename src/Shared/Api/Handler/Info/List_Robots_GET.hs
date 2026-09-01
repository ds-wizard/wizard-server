module Shared.Api.Handler.Info.List_Robots_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState

type List_Robots_GET =
  Header "Host" String
    :> "robots.txt"
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

list_robots_GET :: WizardHandlerC s sm r rm => Maybe String -> sm (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
list_robots_GET mServerUrl =
  runInUnauthService mServerUrl NoTransaction $ do
    let file = "User-agent: *\nDisallow: /"
    traceUuid <- asks (.traceUuid')
    return . addHeader (U.toString traceUuid) . addHeader "text/plain" . FileStream $ file
