module Shared.Model.Cache.ServerCache where

import qualified Data.Cache as C

import Shared.Model.User.UserToken
import Shared.Model.Websocket.WebsocketRecord

data ServerCache = ServerCache
  { knowledgeModelEditorWebsocket :: C.Cache Int WebsocketRecord
  , projectWebsocket :: C.Cache Int WebsocketRecord
  , userToken :: C.Cache Int UserToken
  }
