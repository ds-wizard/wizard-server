module Shared.Model.Websocket.WebsocketSubscription where

import GHC.Generics

data WebsocketSubscription
  = ProjectWebsocketSubscription
  | KnowledgeModelEditorWebsocketSubscription
  | AnalyticsWebsocketSubscription
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

websocketSubscriptionName :: WebsocketSubscription -> String
websocketSubscriptionName ProjectWebsocketSubscription = "Project"
websocketSubscriptionName KnowledgeModelEditorWebsocketSubscription = "KnowledgeModelEditor"
websocketSubscriptionName AnalyticsWebsocketSubscription = "Analytics"

parseWebsocketSubscription :: String -> Maybe WebsocketSubscription
parseWebsocketSubscription name = lookup name [(websocketSubscriptionName s, s) | s <- [minBound .. maxBound]]
