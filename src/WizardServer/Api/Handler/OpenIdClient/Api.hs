module WizardServer.Api.Handler.OpenIdClient.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.OpenIdClient.Detail_DELETE
import WizardServer.Api.Handler.OpenIdClient.Detail_GET
import WizardServer.Api.Handler.OpenIdClient.Detail_Logout_GET
import WizardServer.Api.Handler.OpenIdClient.Detail_PUT
import WizardServer.Api.Handler.OpenIdClient.Detail_Request_GET
import WizardServer.Api.Handler.OpenIdClient.Detail_Response_GET
import WizardServer.Api.Handler.OpenIdClient.List_GET
import WizardServer.Api.Handler.OpenIdClient.List_POST

type OpenIdClientAPI =
  Tags "OpenID Client"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> Detail_Request_GET
           :<|> Detail_Response_GET
           :<|> Detail_Logout_GET
       )

openIdClientApi :: Proxy OpenIdClientAPI
openIdClientApi = Proxy

openIdClientServer :: WizardHandlerC s sm r rm => ServerT OpenIdClientAPI sm
openIdClientServer =
  list_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> detail_request_GET
    :<|> detail_response_GET
    :<|> detail_logout_GET
