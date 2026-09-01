module Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.Common where

import Data.Aeson
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Network.WebSockets
import System.Timeout
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Cache.KnowledgeModelEditorWebsocketCache
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Config.WizardServerConfig
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.Websocket.WebsocketRecord
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Shared.Service.KnowledgeModel.Editor.EditorService
import Shared.Util.JSON
import Shared.Util.String
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertCountOfWebsocketConnection requestContext expCount = do
  (Right resCount) <- runInContext countCache requestContext
  resCount `shouldBe` expCount

-- --------------------------------
-- URL
-- --------------------------------
reqUrlT bUuid mUser =
  let suffix =
        case mUser of
          Just user -> "?Authorization=Bearer%20" ++ user
          Nothing -> ""
   in f' "/wizard-api/knowledge-model-editors/%s/websocket%s" [U.toString bUuid, suffix]

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
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO)
  let (Right (Success_ServerActionDTO (SetUserList_ServerKnowledgeModelEditorMessageDTO resConnection))) = eResult
  length resConnection `shouldBe` expConnectionCount

read_Error connection expError = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String Error_ServerActionDTO
  let (Right (Error_ServerActionDTO error)) = eResult
  error `shouldBe` expError

read_SetUserList_or_Error connection expError = do
  resDto <- receiveData connection
  let (Right result) = eitherDecode resDto :: Either String Object
  let resultType = getField "type" result return
  case resultType of
    (Right "Success_ServerAction") -> read_Error connection expError
    (Right "Error_ServerAction") -> do
      let eResult = eitherDecode resDto :: Either String Error_ServerActionDTO
      let (Right (Error_ServerActionDTO error)) = eResult
      error `shouldBe` expError
    rest -> print rest

nothingWasReceived connection = do
  maybeResDto <- timeout 1000 (receive connection)
  maybeResDto `shouldBe` Nothing

-- --------------------------------
-- DISCONNECT
-- --------------------------------
clearConnections :: RequestContext -> U.UUID -> IO ()
clearConnections requestContext bUuid = do
  (Right records) <- runInContext getAllFromCache requestContext
  traverse_ (clearConnection requestContext bUuid) . filter (\r -> r.entityId == U.toString bUuid) $ records

clearConnection :: RequestContext -> U.UUID -> WebsocketRecord -> IO ()
clearConnection requestContext bUuid record = do
  runInContext (deleteUser bUuid record.connectionUuid) requestContext
  return ()
