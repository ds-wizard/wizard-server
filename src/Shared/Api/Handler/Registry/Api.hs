module Shared.Api.Handler.Registry.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Registry.List_Confirmation_POST
import Shared.Api.Handler.Registry.List_Signup_POST
import Shared.Api.Handler.WizardCommon

type RegistryAPI =
  Tags "Registry"
    :> ( List_Signup_POST
           :<|> List_Confirmation_POST
       )

registryApi :: Proxy RegistryAPI
registryApi = Proxy

registryServer :: WizardHandlerC s sm r rm => ServerT RegistryAPI sm
registryServer = list_signup_POST :<|> list_confirmation_POST
