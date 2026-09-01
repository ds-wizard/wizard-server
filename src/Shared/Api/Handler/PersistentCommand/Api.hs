module Shared.Api.Handler.PersistentCommand.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.PersistentCommand.Detail_Attempts_POST
import Shared.Api.Handler.PersistentCommand.Detail_GET
import Shared.Api.Handler.PersistentCommand.Detail_PUT
import Shared.Api.Handler.PersistentCommand.List_Attempts_POST
import Shared.Api.Handler.PersistentCommand.List_GET
import Shared.Api.Handler.WizardCommon

type PersistentCommandAPI =
  Tags "Persistent Command"
    :> ( List_GET
           :<|> List_Attempts_POST
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_Attempts_POST
       )

persistentCommandApi :: Proxy PersistentCommandAPI
persistentCommandApi = Proxy

persistentCommandServer :: WizardHandlerC s sm r rm => ServerT PersistentCommandAPI sm
persistentCommandServer = list_GET :<|> list_attempts_POST :<|> detail_GET :<|> detail_PUT :<|> detail_attempts_POST
