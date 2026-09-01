module WizardServer.Api.Handler.Role.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.Role.Detail_DELETE
import WizardServer.Api.Handler.Role.Detail_GET
import WizardServer.Api.Handler.Role.Detail_PUT
import WizardServer.Api.Handler.Role.List_GET
import WizardServer.Api.Handler.Role.List_POST

type RoleAPI =
  Tags "Role"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
       )

roleApi :: Proxy RoleAPI
roleApi = Proxy

roleServer :: WizardHandlerC s sm r rm => ServerT RoleAPI sm
roleServer =
  list_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
