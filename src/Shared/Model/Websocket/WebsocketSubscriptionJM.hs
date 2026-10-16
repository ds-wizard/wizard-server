module Shared.Model.Websocket.WebsocketSubscriptionJM where

import Data.Aeson
import qualified Data.Text as T

import Shared.Model.Websocket.WebsocketSubscription

instance ToJSON WebsocketSubscription where
  toJSON = toJSON . websocketSubscriptionName

instance FromJSON WebsocketSubscription where
  parseJSON = withText "WebsocketSubscription" $ \name ->
    maybe (fail "Unknown websocket subscription") return (parseWebsocketSubscription (T.unpack name))
