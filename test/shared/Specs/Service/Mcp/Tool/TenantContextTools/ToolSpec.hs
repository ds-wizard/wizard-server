module Specs.Service.Mcp.Tool.TenantContextTools.ToolSpec (
  tenantContextToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.TenantContextTools.GetTenantContext

tenantContextToolsSpec :: RequestContext -> SpecWith ((), Application)
tenantContextToolsSpec requestContext = do
  get_tenant_context requestContext
