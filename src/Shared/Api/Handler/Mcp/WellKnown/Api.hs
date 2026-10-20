module Shared.Api.Handler.Mcp.WellKnown.Api where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.Mcp.Detail_AuthorizationServer_GET
import Shared.Api.Handler.Mcp.Detail_OpenIdConfiguration_GET
import Shared.Api.Handler.Mcp.Detail_ProtectedResource_GET
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Mcp.Metadata.McpMetadataDTO
import Shared.Api.Resource.Mcp.Metadata.McpProtectedResourceDTO

type WellKnown_ProtectedResource_GET =
  Header "Host" String
    :> ".well-known"
    :> "oauth-protected-resource"
    :> "api"
    :> "mcp"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] McpProtectedResourceDTO)

type WellKnown_AuthorizationServer_GET =
  Header "Host" String
    :> ".well-known"
    :> "oauth-authorization-server"
    :> "api"
    :> "mcp"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] McpMetadataDTO)

type WellKnown_OpenIdConfiguration_GET =
  Header "Host" String
    :> ".well-known"
    :> "openid-configuration"
    :> "api"
    :> "mcp"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] McpMetadataDTO)

type McpWellKnownAPI =
  WellKnown_ProtectedResource_GET
    :<|> WellKnown_AuthorizationServer_GET
    :<|> WellKnown_OpenIdConfiguration_GET

mcpWellKnownApi :: Proxy McpWellKnownAPI
mcpWellKnownApi = Proxy

mcpWellKnownServer :: WizardHandlerC s sm r rm => ServerT McpWellKnownAPI sm
mcpWellKnownServer =
  detail_protectedResource_GET
    :<|> detail_authorizationServer_GET
    :<|> detail_openIdConfiguration_GET
