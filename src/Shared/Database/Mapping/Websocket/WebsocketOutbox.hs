module Shared.Database.Mapping.Websocket.WebsocketOutbox where

import Database.PostgreSQL.Simple

import Shared.Model.Websocket.WebsocketOutbox

instance ToRow WebsocketOutbox

instance FromRow WebsocketOutbox
