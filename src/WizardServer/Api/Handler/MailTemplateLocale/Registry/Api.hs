module WizardServer.Api.Handler.MailTemplateLocale.Registry.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.MailTemplateLocale.Registry.Detail_Pull_POST
import WizardServer.Api.Handler.MailTemplateLocale.Registry.List_GET

type MailTemplateLocaleRegistryAPI =
  Tags "Mail Template Locale"
    :> ( List_GET
           :<|> Detail_Pull_POST
       )

mailTemplateLocaleRegistryApi :: Proxy MailTemplateLocaleRegistryAPI
mailTemplateLocaleRegistryApi = Proxy

mailTemplateLocaleRegistryServer :: WizardHandlerC s sm r rm => ServerT MailTemplateLocaleRegistryAPI sm
mailTemplateLocaleRegistryServer = list_GET :<|> detail_pull_POST
