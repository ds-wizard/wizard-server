module RegistryPublic.Api.Handler.Locale.List_GET where

import Servant

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Locale.LocaleJM ()
import Shared.Api.Handler.Common

type List_GET =
  Header "Authorization" String
    :> "locales"
    :> QueryParam "organizationId" String
    :> QueryParam "templateId" String
    :> QueryParam "recommendedAppVersion" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [LocaleDTO])

list_GET_Api :: Proxy List_GET
list_GET_Api = Proxy
