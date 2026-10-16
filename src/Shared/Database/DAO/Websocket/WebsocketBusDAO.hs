module Shared.Database.DAO.Websocket.WebsocketBusDAO where

import Control.Monad (void, when)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isNothing)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import System.Timeout (timeout)

import Shared.Constant.Websocket
import Shared.Database.DAO.Common
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO (createChannelListener)
import Shared.Model.Context.RequestContext

listenWebsocketChannel :: RequestContextC s sc m => m ()
listenWebsocketChannel = createChannelListener websocketChannelName

waitForWebsocketNotification :: RequestContextC s sc m => Int -> m (Maybe Notification)
waitForWebsocketNotification micros =
  runDB $ \conn -> do
    mNotification <- timeout micros (getNotification conn)
    when (isNothing mNotification) (checkListeningConnection micros conn)
    return mNotification

checkListeningConnection :: Int -> Connection -> IO ()
checkListeningConnection micros conn = do
  result <- timeout micros (query_ conn "SELECT 1" :: IO [Only Int])
  when (isNothing result) (fail "The websocket bus connection does not respond")

notifyWebsocketChannel :: RequestContextC s sc m => BSL.ByteString -> m ()
notifyWebsocketChannel payload = do
  let action conn = query conn "SELECT pg_notify(?, ?)" (websocketChannelName, BSL.toStrict payload) :: IO [Only ()]
  void $ runDB action
