module Shared.Api.Handler.Prefab.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Prefab.List_GET
import Shared.Api.Handler.WizardCommon

type PrefabAPI =
  Tags "Prefab"
    :> List_GET

prefabApi :: Proxy PrefabAPI
prefabApi = Proxy

prefabServer :: WizardHandlerC s sm r rm => ServerT PrefabAPI sm
prefabServer = list_GET
