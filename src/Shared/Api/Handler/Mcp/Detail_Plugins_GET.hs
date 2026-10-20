module Shared.Api.Handler.Mcp.Detail_Plugins_GET where

import Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Mcp.Plugin.McpPluginClient
import Shared.Api.Resource.Mcp.Toolset.McpToolsetJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Mcp.McpToolset
import Shared.Service.Mcp.McpPluginService
import Shared.Util.String (f')

type Detail_Plugins_GET =
  Header "Host" String
    :> "mcp"
    :> "plugins"
    :> Capture "client" McpPluginClient
    :> QueryParam "toolsets" [McpToolset]
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String, Header "Content-Disposition" String] FileStreamLazy)

detail_plugins_GET
  :: WizardHandlerC s sm r rm
  => BS.ByteString
  -> Maybe String
  -> McpPluginClient
  -> Maybe [McpToolset]
  -> sm (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String, Header "Content-Disposition" String] FileStreamLazy)
detail_plugins_GET logo mServerUrl client mToolsets =
  runInUnauthService mServerUrl NoTransaction $ do
    (fileName, archive) <- getMcpPlugin logo client mToolsets
    traceUuid <- asks (.traceUuid')
    return
      . addHeader (U.toString traceUuid)
      . addHeader "application/zip"
      . addHeader (f' "attachment; filename=\"%s\"" [fileName])
      . FileStreamLazy
      $ archive
