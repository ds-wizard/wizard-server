module WizardServer.Api.Handler.Locale.Registry.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.Locale.Registry.Detail_Pull_POST
import WizardServer.Api.Handler.Locale.Registry.List_GET

type LocaleRegistryAPI =
  Tags "Locale"
    :> ( List_GET
           :<|> Detail_Pull_POST
       )

localeRegistryApi :: Proxy LocaleRegistryAPI
localeRegistryApi = Proxy

localeRegistryServer :: WizardHandlerC s sm r rm => ServerT LocaleRegistryAPI sm
localeRegistryServer = list_GET :<|> detail_pull_POST
