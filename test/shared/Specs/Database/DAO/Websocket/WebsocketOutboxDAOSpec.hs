module Specs.Database.DAO.Websocket.WebsocketOutboxDAOSpec where

import Data.Aeson (object, (.=))
import Database.PostgreSQL.Simple (execute_)
import Test.Hspec

import Shared.Constant.Tenant
import Shared.Database.DAO.Common
import Shared.Database.DAO.Websocket.WebsocketOutboxDAO
import Shared.Model.Websocket.WebsocketOutbox
import Shared.Util.Uuid

import Specs.Common

outbox1 =
  WebsocketOutbox
    { uuid = u' "5e000000-0000-0000-0000-000000000001"
    , tenantUuid = defaultTenantUuid
    , payload = object ["type" .= "Broadcast", "data" .= [1 :: Int, 2, 3]]
    }

websocketOutboxDAOSpec requestContext =
  describe "WebsocketOutbox DAO" $ do
    it "finds an inserted row by uuid" $ do
      (Right _) <- runInContext (insertWebsocketOutbox outbox1) requestContext
      (Right found) <- runInContext (findWebsocketOutboxByUuid' outbox1.uuid) requestContext
      found `shouldBe` Just outbox1
      (Right missing) <- runInContext (findWebsocketOutboxByUuid' (u' "5e000000-0000-0000-0000-000000000009")) requestContext
      missing `shouldBe` Nothing
    it "deletes the rows older than 5 minutes" $ do
      (Right _) <- runInContext (insertWebsocketOutbox outbox1) requestContext
      (Right kept) <- runInContext deleteOldWebsocketOutboxes requestContext
      kept `shouldBe` 0
      (Right _) <- runInContext (runDB (`execute_` "UPDATE websocket_outbox SET created_at = now() - interval '6 minutes'")) requestContext
      (Right deleted) <- runInContext deleteOldWebsocketOutboxes requestContext
      deleted `shouldBe` 1
