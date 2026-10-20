module Specs.Service.Mcp.Tool.TenantTools.ToolSpec (
  tenantToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.TenantTools.DeleteTenant
import Specs.Service.Mcp.Tool.TenantTools.GetTenant
import Specs.Service.Mcp.Tool.TenantTools.ListTenants
import Specs.Service.Mcp.Tool.TenantTools.SaveTenant
import Specs.Service.Mcp.Tool.TenantTools.UpdateTenantLimits

tenantToolsSpec :: RequestContext -> SpecWith ((), Application)
tenantToolsSpec requestContext = do
  list_tenants requestContext
  get_tenant requestContext
  save_tenant requestContext
  delete_tenant requestContext
  update_tenant_limits requestContext
