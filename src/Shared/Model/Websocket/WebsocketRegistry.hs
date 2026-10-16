module Shared.Model.Websocket.WebsocketRegistry where

import Control.Concurrent.STM (TBQueue, TMVar, TVar)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.UUID as U
import Data.Word (Word16)

import Shared.Model.User.OnlineUserInfo
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketSubscription

type WebsocketEntityKey = (U.UUID, WebsocketSubscription, U.UUID)

data WebsocketOutbound = WebsocketOutbound
  { queue :: TBQueue BSL.ByteString
  , closeSignal :: TMVar (Word16, Maybe BSL.ByteString)
  , finished :: TMVar ()
  }

data LocalWebsocketConnection = LocalWebsocketConnection
  { connectionUuid :: U.UUID
  , outbound :: WebsocketOutbound
  , onlineUser :: OnlineUserInfo
  , websocketPerm :: WebsocketPerm
  , userGroupUuids :: [U.UUID]
  , tenantRolePermissions :: [String]
  }

data WebsocketRegistry = WebsocketRegistry
  { connections :: TVar (M.Map WebsocketEntityKey (M.Map U.UUID LocalWebsocketConnection))
  , pendingUserLists :: TVar (S.Set WebsocketEntityKey)
  , listenCount :: TVar Int
  }
