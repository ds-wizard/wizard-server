module WizardServer.Api.Handler.Bootstrap.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Bootstrap.List_Workspace_GET
import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.Bootstrap.List_GET

type BootstrapAPI =
  Tags "Bootstrap"
    :> ( List_GET
           :<|> List_Workspace_GET
       )

bootstrapApi :: Proxy BootstrapAPI
bootstrapApi = Proxy

bootstrapServer :: WizardHandlerC s sm r rm => ServerT BootstrapAPI sm
bootstrapServer = list_GET :<|> list_workspace_GET
