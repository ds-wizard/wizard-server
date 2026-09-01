module Wizard.Api.Web where

import Servant

import Shared.Common.Api.Handler.Root.Api
import Shared.Common.Bootstrap.Web
import Shared.Common.Model.Config.BuildInfoConfig
import Wizard.Api.Handler.Api
import Wizard.Api.Handler.Swagger.Api
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext

type WebAPI = RootAPI :<|> ("wizard-api" :> (SwaggerAPI :<|> ApplicationAPI))

webApi :: Proxy WebAPI
webApi = Proxy

webServer :: BaseContext -> Server WebAPI
webServer baseContext =
  hoistServer rootApi (convert baseContext runBaseContextM) (rootServer "wizard")
    :<|> (swaggerServer version :<|> hoistServer applicationApi (convert baseContext runBaseContextM) applicationServer)
  where
    version = baseContext.buildInfoConfig.releaseVersion
