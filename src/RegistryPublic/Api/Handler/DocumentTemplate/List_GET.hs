module RegistryPublic.Api.Handler.DocumentTemplate.List_GET where

import Servant

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Shared.Api.Handler.Common
import Shared.Model.Common.SemVer2Tuple

type List_GET =
  Header "Authorization" String
    :> "document-templates"
    :> QueryParam "organizationId" String
    :> QueryParam "templateId" String
    :> QueryParam "metamodelVersion" SemVer2Tuple
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [DocumentTemplateSimpleDTO])

type Templates__List_GET =
  Header "Authorization" String
    :> "templates"
    :> QueryParam "organizationId" String
    :> QueryParam "templateId" String
    :> QueryParam "metamodelVersion" SemVer2Tuple
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [DocumentTemplateSimpleDTO])

list_GET_Api :: Proxy List_GET
list_GET_Api = Proxy
