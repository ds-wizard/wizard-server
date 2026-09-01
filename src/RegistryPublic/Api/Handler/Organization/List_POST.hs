module RegistryPublic.Api.Handler.Organization.List_POST where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationCreateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationCreateJM ()
import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import Shared.Api.Handler.Common

type List_POST =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] OrganizationCreateDTO
    :> "organizations"
    :> QueryParam "callback" String
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

list_POST_Api :: Proxy List_POST
list_POST_Api = Proxy
