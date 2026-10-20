module Specs.Service.Mcp.Tool.LocaleTools.ToolSpec (
  localeToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.LocaleTools.DeleteLocale
import Specs.Service.Mcp.Tool.LocaleTools.GetLocaleTranslation
import Specs.Service.Mcp.Tool.LocaleTools.ListLocales
import Specs.Service.Mcp.Tool.LocaleTools.SaveLocale

localeToolsSpec :: RequestContext -> SpecWith ((), Application)
localeToolsSpec requestContext = do
  list_locales requestContext
  get_locale_translation requestContext
  save_locale requestContext
  delete_locale requestContext
