module Shared.Api.Handler.Dev.Operation.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Dev.Operation.List_Executions_POST
import Shared.Api.Handler.Dev.Operation.List_GET
import Shared.Api.Handler.WizardCommon

type DevOperationAPI =
  Tags "Dev Operation"
    :> ( List_GET
           :<|> List_Executions_POST
       )

devOperationApi :: Proxy DevOperationAPI
devOperationApi = Proxy

devOperationServer :: WizardHandlerC s sm r rm => ServerT DevOperationAPI sm
devOperationServer = list_GET :<|> list_executions_POST
