module Shared.Database.Mapping.Websocket.WebsocketConnection where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Api.Resource.User.OnlineUserInfoJM ()
import Shared.Database.Mapping.Common ()
import Shared.Model.Websocket.WebsocketConnection
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketSubscription

instance ToField WebsocketSubscription where
  toField = toField . websocketSubscriptionName

instance FromField WebsocketSubscription where
  fromField f dat = do
    name <- fromField f dat
    maybe (returnError ConversionFailed f name) return (parseWebsocketSubscription name)

instance ToField WebsocketPerm where
  toField = toField . websocketPermName

instance FromField WebsocketPerm where
  fromField f dat = do
    name <- fromField f dat
    maybe (returnError ConversionFailed f name) return (parseWebsocketPerm name)

instance ToRow WebsocketConnection where
  toRow WebsocketConnection {..} =
    [ toField uuid
    , toField nodeUuid
    , toField tenantUuid
    , toField subscription
    , toField entityUuid
    , toField userUuid
    , toField websocketPerm
    , toField userGroupUuids
    , toJSONField onlineUser
    ]

instance FromRow WebsocketConnection where
  fromRow = do
    uuid <- field
    nodeUuid <- field
    tenantUuid <- field
    subscription <- field
    entityUuid <- field
    userUuid <- field
    websocketPerm <- field
    userGroupUuids <- fromPGArray <$> field
    onlineUser <- fieldWith fromJSONField
    return $ WebsocketConnection {..}
