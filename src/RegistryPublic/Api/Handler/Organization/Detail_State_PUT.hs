module RegistryPublic.Api.Handler.Organization.Detail_State_PUT where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import RegistryPublic.Api.Resource.Organization.OrganizationStateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationStateJM ()
import Shared.Api.Handler.Common

type Detail_State_PUT =
  ReqBody '[SafeJSON] OrganizationStateDTO
    :> "organizations"
    :> Capture "orgId" String
    :> "state"
    :> QueryParam' '[Required] "hash" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

detail_state_PUT_Api :: Proxy Detail_State_PUT
detail_state_PUT_Api = Proxy
