module Shared.Api.Handler.Info.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Info.List_GET
import Shared.Api.Handler.Info.List_Robots_GET
import Shared.Api.Handler.WizardCommon

type InfoAPI =
  Tags "Info"
    :> (List_GET :<|> List_Robots_GET)

infoApi :: Proxy InfoAPI
infoApi = Proxy

infoServer :: WizardHandlerC s sm r rm => ServerT InfoAPI sm
infoServer = list_GET :<|> list_robots_GET
