module Shared.Api.Handler.MailTemplateLocale.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.MailTemplateLocale.Detail_DELETE
import Shared.Api.Handler.MailTemplateLocale.Detail_PUT
import Shared.Api.Handler.MailTemplateLocale.Detail_Po_GET
import Shared.Api.Handler.MailTemplateLocale.List_GET
import Shared.Api.Handler.MailTemplateLocale.List_POST
import Shared.Api.Handler.MailTemplateLocale.List_Pot_GET
import Shared.Api.Handler.MailTemplateLocale.List_Pot_Info_GET
import Shared.Api.Handler.WizardCommon

type MailTemplateLocaleAPI =
  Tags "Mail Template Locale"
    :> ( List_GET
           :<|> List_POST
           :<|> List_Pot_GET
           :<|> List_Pot_Info_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> Detail_Po_GET
       )

mailTemplateLocaleApi :: Proxy MailTemplateLocaleAPI
mailTemplateLocaleApi = Proxy

mailTemplateLocaleServer :: WizardHandlerC s sm r rm => ServerT MailTemplateLocaleAPI sm
mailTemplateLocaleServer =
  list_GET
    :<|> list_POST
    :<|> list_pot_GET
    :<|> list_pot_info_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> detail_po_GET
