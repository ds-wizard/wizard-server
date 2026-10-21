module Shared.Service.Mcp.Tool.OpenIdClientTools where

import Control.Monad (void, when)
import Control.Monad.Except (liftEither)
import Data.Aeson
import Data.Proxy
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM ()
import Shared.Database.DAO.OpenId.OpenIdClientDefinitionDAO
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.OpenId.OpenIdClient
import Shared.Model.OpenId.OpenIdClientStyle
import Shared.Model.User.RolePermission
import Shared.Service.Mcp.Tool.McpAdminLogin
import Shared.Service.Mcp.Tool.McpExecutableServices
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.OpenId.Client.Definition.OpenIdClientDefinitionMapper (toChangeDTO, toMaskedDetailDTO)
import Shared.Util.JsonSchema
import Shared.Util.Secret

openIdClientTools :: WizardRequestContextC s m => McpExecutableServices m -> [McpTool m]
openIdClientTools services =
  [ listOpenIdClientsTool
  , saveOpenIdClientTool services.createOpenIdClient services.modifyOpenIdClient services.findUserLoginProviders
  , deleteOpenIdClientTool services.deleteOpenIdClient services.findUserLoginProviders
  ]

listOpenIdClientsTool :: WizardRequestContextC s m => McpTool m
listOpenIdClientsTool =
  McpTool
    { name = "list_open_id_clients"
    , title = "List OpenID clients"
    , description = "List the OpenID clients users log in with (single sign-on), with their settings; the client secrets show as ********"
    , inputSchema = argumentsSchema (pure ())
    , outputSchema = Just $ objectSchema ["openIdClients"] [("openIdClients", arraySchema (jsonSchema (Proxy :: Proxy OpenIdClientDetailDTO)))]
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION, _SETTINGS_MANAGE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments (pure ()) . const $ (\clients -> object ["openIdClients" .= fmap toMaskedDetailDTO clients]) <$> findOpenIdClientDefinitions
    }

saveOpenIdClientTool
  :: forall s m a b
   . WizardRequestContextC s m
  => (U.UUID -> OpenIdClientChangeDTO -> m a)
  -> (U.UUID -> OpenIdClientChangeDTO -> m b)
  -> (U.UUID -> m [U.UUID])
  -> McpTool m
saveOpenIdClientTool createClient modifyClient findUserLoginProviders =
  McpTool
    { name = "save_open_id_client"
    , title = "Save OpenID client"
    , description =
        "Create an OpenID client users log in with (single sign-on) or change its settings; only the fields you give change. The client secret is write-only: "
          ++ secretDescription
          ++ ", except in a call that changes url. A new client needs name, url (the issuer), clientId and clientSecret. "
          ++ uuidDescription "a client"
          ++ ". "
          ++ expectedUpdatedAtDescription "client"
          ++ ". "
          ++ "Changing url or clientId is refused when no active admin could log in any more without the client"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy OpenIdClientDetailDTO)
    , annotations = writeAnnotations
    , continuable = False
    , permissions = loginProviderWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(mUuid, mExpectedUpdatedAt, changes) ->
        toJSON . toMaskedDetailDTO <$> saveOpenIdClient createClient modifyClient findUserLoginProviders mUuid mExpectedUpdatedAt changes
    }
  where
    arguments =
      (,,)
        <$> uuidArgument clientEntityName
        <*> expectedUpdatedAtArgument
        <*> changesArguments (jsonSchema (Proxy :: Proxy OpenIdClientChangeDTO))
        :: Arguments (Maybe U.UUID, Maybe UTCTime, Object)

saveOpenIdClient
  :: WizardRequestContextC s m
  => (U.UUID -> OpenIdClientChangeDTO -> m a)
  -> (U.UUID -> OpenIdClientChangeDTO -> m b)
  -> (U.UUID -> m [U.UUID])
  -> Maybe U.UUID
  -> Maybe UTCTime
  -> Object
  -> m OpenIdClient
saveOpenIdClient createClient modifyClient findUserLoginProviders mUuid mExpectedUpdatedAt changes =
  runInTransaction $ do
    let update = updateClientFromChanges modifyClient findUserLoginProviders mExpectedUpdatedAt changes
    uuid <- saveByUuid findOpenIdClientDefinitionByUuid' (createClientFromChanges createClient changes) update mUuid
    findOpenIdClientDefinitionByUuid uuid

createClientFromChanges :: WizardRequestContextC s m => (U.UUID -> OpenIdClientChangeDTO -> m a) -> Object -> U.UUID -> m U.UUID
createClientFromChanges createClient changes uuid = do
  let defaults = OpenIdClientChangeDTO "" "" "" "" [] (OpenIdClientStyle Nothing Nothing Nothing) True True True
  uuid <$ createFromChanges clientEntityName ["name", "url", "clientId", "clientSecret"] (`mergeChanges` defaults) (createClient uuid) changes

updateClientFromChanges
  :: WizardRequestContextC s m
  => (U.UUID -> OpenIdClientChangeDTO -> m b)
  -> (U.UUID -> m [U.UUID])
  -> Maybe UTCTime
  -> Object
  -> OpenIdClient
  -> m U.UUID
updateClientFromChanges modifyClient findUserLoginProviders mExpectedUpdatedAt changes client = do
  let current = toChangeDTO client
  changed <- mergeChangesSince clientEntityName mExpectedUpdatedAt client.updatedAt changes current
  when ((changed.url, changed.clientId) /= (current.url, current.clientId)) (checkAdminLoginRemains findUserLoginProviders client.uuid)
  clientSecret <- liftEither (unmaskFor (changed.url /= current.url) "url" current.clientSecret changed.clientSecret)
  let unmasked = changed {clientSecret = clientSecret} :: OpenIdClientChangeDTO
  client.uuid <$ modifyIfChanged current unmasked (return ()) (void . modifyClient client.uuid)

deleteOpenIdClientTool :: WizardRequestContextC s m => (U.UUID -> m ()) -> (U.UUID -> m [U.UUID]) -> McpTool m
deleteOpenIdClientTool deleteClient findUserLoginProviders =
  McpTool
    { name = "delete_open_id_client"
    , title = "Delete OpenID client"
    , description =
        "Delete an OpenID client; its users can no longer log in through it. It is refused when no active admin could log in any more, "
          ++ "because none of them has a password or another single sign-on. It cannot be undone"
    , inputSchema = argumentsSchema clientUuidArgument
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = loginProviderWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments clientUuidArgument $ \uuid -> object [] <$ deleteLoginProvider deleteClient findUserLoginProviders uuid
    }

deleteLoginProvider :: WizardRequestContextC s m => (U.UUID -> m ()) -> (U.UUID -> m [U.UUID]) -> U.UUID -> m ()
deleteLoginProvider deleteProvider findUserLoginProviders uuid =
  runInTransaction $ do
    checkAdminLoginRemains findUserLoginProviders uuid
    deleteProvider uuid

loginProviderWritePermissions :: [String]
loginProviderWritePermissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION, _SETTINGS_MANAGE_ROLE_PERMISSION]

clientUuidArgument :: Arguments U.UUID
clientUuidArgument = requiredArgument "open_id_client_uuid" "The OpenID client"

clientEntityName :: String
clientEntityName = "OpenID client"
