module Specs.Service.Mcp.Tool.MailTemplateLocaleTools.ToolSpec (
  mailTemplateLocaleToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.MailTemplateLocaleTools.DeleteMailTemplateLocale
import Specs.Service.Mcp.Tool.MailTemplateLocaleTools.GetMailTemplateTranslation
import Specs.Service.Mcp.Tool.MailTemplateLocaleTools.ListMailTemplateLocales
import Specs.Service.Mcp.Tool.MailTemplateLocaleTools.SaveMailTemplateLocale

mailTemplateLocaleToolsSpec :: RequestContext -> SpecWith ((), Application)
mailTemplateLocaleToolsSpec requestContext = do
  list_mail_template_locales requestContext
  get_mail_template_translation requestContext
  save_mail_template_locale requestContext
  delete_mail_template_locale requestContext
