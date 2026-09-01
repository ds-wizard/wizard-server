module RegistryPublic.Api.Handler.Organization.List_Simple_GET where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryPublic.Model.Organization.OrganizationSimple
import Shared.Api.Handler.Common

type List_Simple_GET =
  "organizations"
    :> "simple"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [OrganizationSimple])

list_simple_GET_Api :: Proxy List_Simple_GET
list_simple_GET_Api = Proxy
