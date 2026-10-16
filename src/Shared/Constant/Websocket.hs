module Shared.Constant.Websocket where

import Data.Int (Int64)

websocketChannelName :: String
websocketChannelName = "websocket"

websocketOutboundQueueSize :: Int
websocketOutboundQueueSize = 256

websocketInboundMessageSizeLimit :: Int64
websocketInboundMessageSizeLimit = 1024 * 1024

websocketInlinePayloadLimit :: Int64
websocketInlinePayloadLimit = 7 * 1024

websocketPingIntervalSeconds :: Int
websocketPingIntervalSeconds = 30

websocketCloseTimeoutMicros :: Int
websocketCloseTimeoutMicros = 2000000

websocketShutdownDrainMicros :: Int
websocketShutdownDrainMicros = 5000000

websocketUserListDebounceMicros :: Int
websocketUserListDebounceMicros = 200000

websocketListenerLivenessMicros :: Int
websocketListenerLivenessMicros = 30000000

websocketListenerRestartDelayMicros :: Int
websocketListenerRestartDelayMicros = 5000000

websocketHeartbeatMicros :: Int
websocketHeartbeatMicros = 30000000

websocketStaleConnectionInterval :: String
websocketStaleConnectionInterval = "2 minutes"

websocketOutboxRetentionInterval :: String
websocketOutboxRetentionInterval = "5 minutes"
