module WizardServer.Api.Web where

import Servant hiding (ServerContext)

import Shared.Api.Handler.Api
import Shared.Api.Handler.Root.Api
import Shared.Bootstrap.Web
import Shared.Model.Config.BuildInfoConfig
import WizardServer.Api.Handler.Api
import WizardServer.Api.Handler.Common ()
import WizardServer.Api.Handler.Swagger.Api
import WizardServer.Model.Context.ServerContext

type WebAPI = RootAPI :<|> ("wizard-api" :> (SwaggerAPI :<|> ApplicationAPI :<|> ManagementAPI))

webApi :: Proxy WebAPI
webApi = Proxy

webServer :: ServerContext -> Server WebAPI
webServer serverContext =
  hoistServer rootApi (convert serverContext runServerContextM) (rootServer "wizard")
    :<|> ( swaggerServer version
             :<|> hoistServer applicationApi (convert serverContext runServerContextM) applicationServer
             :<|> hoistServer managementApi (convert serverContext runServerContextM) managementServer
         )
  where
    version = serverContext.buildInfoConfig.releaseVersion
