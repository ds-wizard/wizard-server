module Shared.Api.Handler.Project.Event.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Project.Event.Detail_GET
import Shared.Api.Handler.Project.Event.List_GET
import Shared.Api.Handler.Project.Event.List_POST
import Shared.Api.Handler.WizardCommon

type EventAPI =
  Tags "Project Event"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_GET
       )

eventApi :: Proxy EventAPI
eventApi = Proxy

eventServer :: WizardHandlerC s sm r rm => ServerT EventAPI sm
eventServer =
  list_GET
    :<|> list_POST
    :<|> detail_GET
