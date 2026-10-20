module Specs.Service.Mcp.Tool.RegistryLocaleTools.ToolSpec (
  registryLocaleToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.RegistryLocaleTools.ListRegistryLocales
import Specs.Service.Mcp.Tool.RegistryLocaleTools.PullRegistryLocale

registryLocaleToolsSpec :: RequestContext -> SpecWith ((), Application)
registryLocaleToolsSpec requestContext = do
  list_registry_locales requestContext
  pull_registry_locale requestContext
