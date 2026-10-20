module Specs.Service.Mcp.Tool.OpenIdClientTools.ToolSpec (
  openIdClientToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.OpenIdClientTools.DeleteOpenIdClient
import Specs.Service.Mcp.Tool.OpenIdClientTools.ListOpenIdClients
import Specs.Service.Mcp.Tool.OpenIdClientTools.SaveOpenIdClient

openIdClientToolsSpec :: RequestContext -> SpecWith ((), Application)
openIdClientToolsSpec requestContext = do
  list_open_id_clients requestContext
  save_open_id_client requestContext
  delete_open_id_client requestContext
