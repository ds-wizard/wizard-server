module Shared.Api.Handler.Mcp.McpPost where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Aeson as A
import Network.HTTP.Types (methodPost)
import Network.Wai (Response, ResponseReceived)
import Servant
import Servant.Server.Internal (RouteResult (..), addMethodCheck, leafRouter, methodCheck, runDelayed)
import Servant.Swagger (HasSwagger (..))

import Shared.Api.Handler.Common.ContentType
import Shared.Api.Resource.Common.AesonSM ()

data McpPost

instance HasServer McpPost context where
  type ServerT McpPost m = (Response -> IO ResponseReceived) -> m ResponseReceived
  route _ _ action = leafRouter $ \env request respond -> runResourceT $ do
    result <- runDelayed (action `addMethodCheck` methodCheck methodPost request) env request
    liftIO $ case result of
      Route handler -> runHandler (handler (respond . Route)) >>= either (respond . FailFatal) return
      Fail error -> respond (Fail error)
      FailFatal error -> respond (FailFatal error)
  hoistServerWithContext _ _ nt server = nt . server

instance HasSwagger McpPost where
  toSwagger _ = toSwagger (Proxy :: Proxy (Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] A.Value)))
