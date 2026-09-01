module Shared.Service.Websocket.WebsocketService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.UUID as U
import Network.WebSockets (Connection)

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Websocket.WebsocketRecord
import Shared.Service.User.WizardUserMapper
import Shared.Util.Number

createRecord :: WizardRequestContextC s m => U.UUID -> Connection -> String -> WebsocketPerm -> [U.UUID] -> m WebsocketRecord
createRecord connectionUuid connection entityId permission userGroupUuids = do
  mCurrentUser <- asks (.currentUser')
  avatarNumber <- liftIO $ generateInt 20
  colorNumber <- liftIO $ generateInt 12
  let user = toOnlineUserInfo mCurrentUser avatarNumber colorNumber userGroupUuids
  return $ WebsocketRecord connectionUuid connection entityId permission user

filterEditors :: [WebsocketRecord] -> [WebsocketRecord]
filterEditors records =
  let isEditor record = record.entityPerm == EditorWebsocketPerm
   in filter isEditor records

filterCommenters :: [WebsocketRecord] -> [WebsocketRecord]
filterCommenters records =
  let isCommenter record = record.entityPerm == EditorWebsocketPerm || record.entityPerm == CommenterWebsocketPerm
   in filter isCommenter records
