module WizardServer.Api.Handler.Config.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.Config.List_Bootstrap_GET

type ConfigAPI =
  Tags "Config"
    :> List_Bootstrap_GET

configApi :: Proxy ConfigAPI
configApi = Proxy

configServer :: WizardHandlerC s sm r rm => ServerT ConfigAPI sm
configServer = list_bootstrap_GET
