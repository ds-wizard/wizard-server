module Specs.Api.Handler.Project.Detail_WS.Common where

import Control.Concurrent (threadDelay)
import qualified Control.Exception.Base as E
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import qualified Network.HTTP.Client as HC
import qualified Network.Wreq as W
import qualified Network.Wreq.Types as WT
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Constant.Tenant
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Websocket.WebsocketConnectionDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Integration.Http.Common.HttpClient (mapHeader)
import Shared.Integration.Http.Common.HttpClientFactory
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Http.HttpRequest
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Model.Websocket.WebsocketSubscription
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

assertCountOfWebsocketConnectionEventually requestContext =
  assertEventually (fromRight (-1) <$> runInContext countWebsocketConnections requestContext)

-- --------------------------------
-- URL
-- --------------------------------
reqUrlT projectUuid mUser =
  let suffix =
        case mUser of
          Just user -> "?Authorization=Bearer%20" ++ user
          Nothing -> ""
   in f' "/api/projects/%s/websocket%s" [U.toString projectUuid, suffix]

-- --------------------------------
-- DATABASE
-- --------------------------------
insertProjectAndUsers requestContext project =
  -- Prepare DB
  do
    runInContext U.runMigration requestContext
    runInContextIO TML_Migration.runMigration requestContext
    runInContextIO (insertPackage germanyKmPackage) requestContext
    runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
    runInContextIO (insertProject project) requestContext

-- --------------------------------
-- CONNECT
-- --------------------------------
connectTestWebsocketUsers requestContext projectUuid =
  -- Clear websockets
  do
    clearConnections requestContext projectUuid
    -- Connect 1. user
    (c1, s1) <- createConnection requestContext (reqUrlT projectUuid (Just reqAuthToken))
    read_SetUserList c1 0
    -- Connect 2. user
    (c2, s2) <- createConnection requestContext (reqUrlT projectUuid (Just reqNonAdminAuthToken))
    read_SetUserList c1 1
    read_SetUserList c2 1
    -- Connect 3. user
    (c3, s3) <- createConnection requestContext (reqUrlT projectUuid Nothing)
    read_SetUserList c1 2
    read_SetUserList c2 2
    read_SetUserList c3 2
    return ((c1, s1), (c2, s2), (c3, s3))

read_SetUserList connection expConnectionCount = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO)
  let (Right (Success_ServerActionDTO (SetUserList_ServerProjectMessageDTO resConnection))) = eResult
  length resConnection `shouldBe` expConnectionCount

skip_SetProject connection = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO)
  let (Right (Success_ServerActionDTO (SetProject_ServerProjectMessageDTO _))) = eResult
  return ()

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
clearConnections requestContext projectUuid = do
  let key = (defaultTenantUuid, ProjectWebsocketSubscription, projectUuid)
  (Right connections) <- runInContext (findLocalConnections key) requestContext
  traverse_ (clearConnection requestContext key) connections
  threadDelay 300000

clearConnection :: RequestContext -> WebsocketEntityKey -> LocalWebsocketConnection -> IO ()
clearConnection requestContext key connection = do
  runInContext (disconnectUser key connection.connectionUuid >> WS.closeConnection connection 1001 Nothing) requestContext
  return ()

-- --------------------------------
-- HTTP Client
-- --------------------------------
runSimpleRequest :: RequestContext -> HttpRequest -> IO (Either E.SomeException (HC.Response BSL.ByteString))
runSimpleRequest requestContext req = do
  httpClientManager <- createHttpClientManager requestContext.serverConfig.logging
  let opts =
        W.defaults
          { WT.manager = Right httpClientManager
          , WT.headers = reqHeaders
          , WT.checkResponse = Just (\_ _ -> return ())
          }
  E.try . action $ opts
  where
    reqMethod = req.requestMethod
    host =
      if requestContext.serverConfig.general.serverPort == 80
        then "localhost"
        else "localhost:" ++ show requestContext.serverConfig.general.serverPort
    reqUrl = f' "http://%s/%s" [host, req.requestUrl]
    reqHeaders = mapHeader <$> M.toList req.requestHeaders
    action opts
      | reqMethod == "GET" = W.getWith opts reqUrl
      | otherwise = W.customPayloadMethodWith reqMethod opts reqUrl req.requestBody
