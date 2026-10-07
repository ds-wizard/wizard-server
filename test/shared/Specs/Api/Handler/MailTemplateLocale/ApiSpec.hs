module Specs.Api.Handler.MailTemplateLocale.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common

import Specs.Api.Handler.MailTemplateLocale.Detail_DELETE
import Specs.Api.Handler.MailTemplateLocale.Detail_PUT
import Specs.Api.Handler.MailTemplateLocale.Detail_Po_GET
import Specs.Api.Handler.MailTemplateLocale.List_GET
import Specs.Api.Handler.MailTemplateLocale.List_POST
import Specs.Api.Handler.MailTemplateLocale.List_Pot_GET
import Specs.Api.Handler.MailTemplateLocale.List_Pot_Info_GET

mailTemplateLocaleAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "MAIL TEMPLATE LOCALE API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      list_pot_GET requestContext
      list_pot_info_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
      detail_po_GET requestContext
