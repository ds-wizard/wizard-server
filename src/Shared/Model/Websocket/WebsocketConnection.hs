module Shared.Model.Websocket.WebsocketConnection where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.User.OnlineUserInfo
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketSubscription

data WebsocketConnection = WebsocketConnection
  { uuid :: U.UUID
  , nodeUuid :: U.UUID
  , tenantUuid :: U.UUID
  , subscription :: WebsocketSubscription
  , entityUuid :: U.UUID
  , userUuid :: Maybe U.UUID
  , websocketPerm :: WebsocketPerm
  , userGroupUuids :: [U.UUID]
  , onlineUser :: OnlineUserInfo
  }
  deriving (Show, Eq, Generic)
