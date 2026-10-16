module Shared.Model.Websocket.WebsocketPerm where

data WebsocketPerm
  = NoWebsocketPerm
  | ViewerWebsocketPerm
  | CommenterWebsocketPerm
  | EditorWebsocketPerm
  deriving (Show, Eq, Enum, Bounded)

websocketPermName :: WebsocketPerm -> String
websocketPermName NoWebsocketPerm = "None"
websocketPermName ViewerWebsocketPerm = "Viewer"
websocketPermName CommenterWebsocketPerm = "Commenter"
websocketPermName EditorWebsocketPerm = "Editor"

parseWebsocketPerm :: String -> Maybe WebsocketPerm
parseWebsocketPerm name = lookup name [(websocketPermName p, p) | p <- [minBound .. maxBound]]
