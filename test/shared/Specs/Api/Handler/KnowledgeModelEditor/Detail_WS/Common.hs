module Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.Common where

import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Constant.Tenant
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Websocket.WebsocketConnectionDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Config.WizardServerConfig
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Model.Websocket.WebsocketSubscription
import Shared.Service.KnowledgeModel.Editor.EditorService
import Shared.Service.Websocket.WebsocketRegistryService
import Shared.Service.Websocket.WebsocketService
import Shared.Util.JSON
import Shared.Util.String
import qualified Shared.Util.Websocket as WS
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertCountOfWebsocketConnection requestContext expCount = do
  (Right resCount) <- runInContext countWebsocketConnections requestContext
  resCount `shouldBe` expCount

-- --------------------------------
-- URL
-- --------------------------------
reqUrlT bUuid mUser =
  let suffix =
        case mUser of
          Just user -> "?Authorization=Bearer%20" ++ user
          Nothing -> ""
   in f' "/api/knowledge-model-editors/%s/websocket%s" [U.toString bUuid, suffix]

-- --------------------------------
-- DATABASE
-- --------------------------------
insertKnowledgeModelEditorAndUsers requestContext editor =
  -- Prepare DB
  do
    runInContext U.runMigration requestContext
    runInContextIO TML_Migration.runMigration requestContext
    runInContextIO (insertPackage germanyKmPackage) requestContext
    runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
    runInContextIO (insertKnowledgeModelEditor editor) requestContext
    runInContextIO
      ( createEditorWithParams
          leidenKnowledgeModelEditor.uuid
          leidenKnowledgeModelEditor.createdAt
          (fromJust requestContext.currentUser)
          leidenKnowledgeModelEditorCreate
      )
      requestContext

-- --------------------------------
-- CONNECT
-- --------------------------------
connectTestWebsocketUsers requestContext bUuid =
  -- Clear websockets
  do
    clearConnections requestContext bUuid
    -- Connect 1. user
    (c1, s1) <- createConnection requestContext (reqUrlT bUuid (Just reqAuthToken))
    read_SetUserList c1 0
    -- Connect 2. user
    (c2, s2) <- createConnection requestContext (reqUrlT bUuid (Just reqNonAdminAuthToken))
    read_SetUserList c1 1
    read_SetUserList c2 1
    return ((c1, s1), (c2, s2))

read_SetUserList connection expConnectionCount = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO)
  let (Right (Success_ServerActionDTO (SetUserList_ServerKnowledgeModelEditorMessageDTO resConnection))) = eResult
  length resConnection `shouldBe` expConnectionCount

read_Error connection expError = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String Error_ServerActionDTO
  let (Right (Error_ServerActionDTO error)) = eResult
  error `shouldBe` expError

read_SetUserList_or_Error connection expError = do
  resDto <- receiveDataWithin connection
  let (Right result) = eitherDecode resDto :: Either String Object
  let resultType = getField "type" result return
  case resultType of
    (Right "Success_ServerAction") -> read_Error connection expError
    (Right "Error_ServerAction") -> do
      let eResult = eitherDecode resDto :: Either String Error_ServerActionDTO
      let (Right (Error_ServerActionDTO error)) = eResult
      error `shouldBe` expError
    rest -> print rest

-- --------------------------------
-- DISCONNECT
-- --------------------------------
clearConnections :: RequestContext -> U.UUID -> IO ()
clearConnections requestContext bUuid = do
  let key = (defaultTenantUuid, KnowledgeModelEditorWebsocketSubscription, bUuid)
  (Right connections) <- runInContext (findLocalConnections key) requestContext
  traverse_ (clearConnection requestContext key) connections
  threadDelay 300000

clearConnection :: RequestContext -> WebsocketEntityKey -> LocalWebsocketConnection -> IO ()
clearConnection requestContext key connection = do
  runInContext (disconnectUser key connection.connectionUuid >> WS.closeConnection connection 1001 Nothing) requestContext
  return ()
