module Shared.Api.Handler.TypeHint.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.TypeHint.List_POST
import Shared.Api.Handler.TypeHint.Test_POST
import Shared.Api.Handler.WizardCommon

type TypeHintAPI =
  Tags "TypeHint"
    :> ( List_POST
           :<|> Test_POST
       )

typeHintApi :: Proxy TypeHintAPI
typeHintApi = Proxy

typeHintServer :: WizardHandlerC s sm r rm => ServerT TypeHintAPI sm
typeHintServer =
  list_POST
    :<|> test_POST
