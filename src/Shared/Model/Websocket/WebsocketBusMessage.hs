module Shared.Model.Websocket.WebsocketBusMessage where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Websocket.WebsocketSubscription
import Shared.Model.Websocket.WebsocketSubscriptionJM ()
import Shared.Util.Aeson

data WebsocketRecipients
  = AllRecipients
  | EditorsOnly
  | CommentersAndEditors
  deriving (Show, Eq, Generic)

data WebsocketBusMessage
  = Broadcast
      { tenantUuid :: U.UUID
      , subscription :: WebsocketSubscription
      , entityUuid :: U.UUID
      , recipients :: WebsocketRecipients
      , payload :: Value
      }
  | UserListChanged
      { tenantUuid :: U.UUID
      , subscription :: WebsocketSubscription
      , entityUuid :: U.UUID
      }
  | ProjectPermissionsChanged
      { tenantUuid :: U.UUID
      , projectUuid :: U.UUID
      }
  | WorkspacePermissionsChanged
      { tenantUuid :: U.UUID
      , workspaceUuid :: U.UUID
      }
  | UserGroupRemoved
      { tenantUuid :: U.UUID
      , userGroupUuid :: U.UUID
      , userUuids :: [U.UUID]
      }
  | LogOutAll
      { tenantUuid :: U.UUID
      , subscription :: WebsocketSubscription
      , entityUuid :: U.UUID
      }
  | AnalyticsEtlFinished
      { tenantUuid :: U.UUID
      }
  | UserTokensDeleted
      { tenantUuid :: U.UUID
      , userTokenUuids :: [U.UUID]
      }
  deriving (Show, Eq, Generic)

instance ToJSON WebsocketRecipients where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON WebsocketRecipients where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON WebsocketBusMessage where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON WebsocketBusMessage where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

data WebsocketBusEnvelope
  = WebsocketBusInline WebsocketBusMessage
  | WebsocketBusOutbox U.UUID
  deriving (Show, Eq, Generic)

instance ToJSON WebsocketBusEnvelope where
  toJSON (WebsocketBusInline message) = toJSON message
  toJSON (WebsocketBusOutbox uuid) = object ["type" .= ("Outbox" :: String), "uuid" .= uuid]

instance FromJSON WebsocketBusEnvelope where
  parseJSON = withObject "WebsocketBusEnvelope" $ \o -> do
    messageType <- o .: "type"
    if messageType == ("Outbox" :: String)
      then WebsocketBusOutbox <$> o .: "uuid"
      else WebsocketBusInline <$> parseJSON (Object o)
