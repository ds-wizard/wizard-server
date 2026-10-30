module Shared.Api.Handler.Token.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Token.Detail_DELETE
import Shared.Api.Handler.Token.List_Current_DELETE
import Shared.Api.Handler.Token.List_DELETE
import Shared.Api.Handler.Token.List_GET
import Shared.Api.Handler.Token.List_POST
import Shared.Api.Handler.WizardCommon

type TokenAPI =
  Tags "Token"
    :> ( List_GET
           :<|> List_POST
           :<|> List_DELETE
           :<|> List_Current_DELETE
           :<|> Detail_DELETE
       )

tokenApi :: Proxy TokenAPI
tokenApi = Proxy

tokenServer :: WizardHandlerC s sm r rm => ServerT TokenAPI sm
tokenServer =
  list_GET
    :<|> list_POST
    :<|> list_DELETE
    :<|> list_current_DELETE
    :<|> detail_DELETE
