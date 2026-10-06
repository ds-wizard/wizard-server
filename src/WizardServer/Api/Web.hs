module WizardServer.Api.Web where

import Servant hiding (ServerContext)

import Shared.Api.Handler.Root.Api
import Shared.Bootstrap.Web
import Shared.Model.Config.BuildInfoConfig
import WizardServer.Api.Handler.Api
import WizardServer.Api.Handler.Common ()
import WizardServer.Api.Handler.Swagger.Api
import WizardServer.Model.Context.ServerContext

type WebAPI = RootAPI :<|> ("api" :> (SwaggerAPI :<|> ApplicationAPI))

webApi :: Proxy WebAPI
webApi = Proxy

webServer :: ServerContext -> Server WebAPI
webServer serverContext =
  hoistServer rootApi run rootServer
    :<|> (swaggerServer version :<|> hoistServer applicationApi run applicationServer)
  where
    run :: ServerContextM a -> Handler a
    run = convert serverContext (.runServerContextM)
    version = serverContext.buildInfoConfig.releaseVersion
