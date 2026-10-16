module Shared.Model.Websocket.WebsocketOutbox where

import Data.Aeson (Value)
import qualified Data.UUID as U
import GHC.Generics

data WebsocketOutbox = WebsocketOutbox
  { uuid :: U.UUID
  , tenantUuid :: U.UUID
  , payload :: Value
  }
  deriving (Show, Eq, Generic)
