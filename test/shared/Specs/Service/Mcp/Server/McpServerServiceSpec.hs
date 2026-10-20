module Specs.Service.Mcp.Server.McpServerServiceSpec where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (isJust, isNothing)
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.PostgreSQL.Simple
import Test.Hspec

import Shared.Api.Resource.Mcp.JsonRpc.JsonRpcResponseJM ()
import Shared.Constant.Mcp
import Shared.Database.DAO.WizardCommon
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpTool
import Shared.Model.Workspace.Workspace
import Shared.Service.Mcp.Server.McpOutputLimit
import Shared.Service.Mcp.Server.McpServerService
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Mcp
import Specs.Common

mcpServerServiceSpec :: RequestContext -> Spec
mcpServerServiceSpec requestContext =
  describe "McpServerService" $
    describe "handleMcpRequest (tools/call)" $ do
      it "gives a transactional tool a pooled connection" $
        -- GIVEN:
        do
          pool <- createSingleConnectionPool requestContext
          let tool = testTool Transactional (insertToolWorkspace >> return (object ["done" .= True]))
          -- WHEN:
          result <- callTestTool tool (withPool pool requestContext)
          -- THEN:
          (Right mWorkspace) <- runInContext (findWorkspaceByUuid' toolWorkspace.uuid) requestContext
          valueField "structuredContent" result `shouldBe` Just (object ["done" .= True])
          isJust mWorkspace `shouldBe` True
          Pool.destroyAllResources pool
      it "answers a tool error when a tool without a connection opens a transaction" $
        -- GIVEN:
        do
          pool <- createSingleConnectionPool requestContext
          let tool = testTool NoTransaction (insertToolWorkspace >> return (object []))
          -- WHEN:
          result <- callTestTool tool (withPool pool requestContext)
          -- THEN:
          (Right mWorkspace) <- runInContext (findWorkspaceByUuid' toolWorkspace.uuid) requestContext
          valueField "isError" result `shouldBe` Just (Bool True)
          isNothing mWorkspace `shouldBe` True
          Pool.destroyAllResources pool
      it "rolls back and returns the connection to the pool when a tool throws" $
        -- GIVEN:
        do
          pool <- createSingleConnectionPool requestContext
          pidBefore <- backendPid pool
          -- WHEN:
          result <- callTestTool (testTool Transactional failingTool) (withPool pool requestContext)
          -- THEN:
          (Right mWorkspace) <- runInContext (findWorkspaceByUuid' toolWorkspace.uuid) requestContext
          pidAfter <- backendPid pool
          valueField "isError" result `shouldBe` Just (Bool True)
          isNothing mWorkspace `shouldBe` True
          pidAfter `shouldBe` pidBefore
          Pool.destroyAllResources pool
      it "cuts a result that is an array to the output limit" $
        -- GIVEN:
        do
          let items = V.replicate 1000 (String (T.replicate 100 "x"))
          -- WHEN:
          result <- callTestTool (testTool NoTransaction (return (Array items))) requestContext
          -- THEN:
          encodedSize result `shouldSatisfy` (<= mcpMaxOutputSize)
          (valueField "truncated" =<< valueField "structuredContent" result) `shouldBe` Just (Bool True)
      it "rolls back a transaction a tool leaves open when it returns" $
        -- GIVEN:
        do
          pool <- createSingleConnectionPool requestContext
          -- WHEN:
          _ <- callTestTool (testTool Transactional (openTransaction >> return (object []))) (withPool pool requestContext)
          -- THEN:
          idle <- isConnectionIdle pool
          idle `shouldBe` True
          Pool.destroyAllResources pool
      it "rolls back a transaction a tool leaves open when it fails with an error" $
        -- GIVEN:
        do
          pool <- createSingleConnectionPool requestContext
          let tool = testTool Transactional (openTransaction >> throwError (UserError (_ERROR_VALIDATION__FORBIDDEN "test")))
          -- WHEN:
          result <- callTestTool tool (withPool pool requestContext)
          -- THEN:
          idle <- isConnectionIdle pool
          valueField "isError" result `shouldBe` Just (Bool True)
          idle `shouldBe` True
          Pool.destroyAllResources pool

testTool :: TransactionState -> RequestContextM Value -> McpTool RequestContextM
testTool transactionState action =
  McpTool
    { name = "test_tool"
    , title = "Test tool"
    , description = "A tool of the spec"
    , inputSchema = object []
    , outputSchema = Nothing
    , annotations = McpToolAnnotations {readOnlyHint = False, destructiveHint = False, idempotentHint = False, openWorldHint = False}
    , continuable = True
    , permissions = []
    , toolset = Nothing
    , transactionState = transactionState
    , run = const (Right action)
    }

callTestTool :: McpTool RequestContextM -> RequestContext -> IO Value
callTestTool tool context = do
  let body = object ["jsonrpc" .= ("2.0" :: String), "id" .= (1 :: Int), "method" .= ("tools/call" :: String), "params" .= object ["name" .= tool.name]]
  (Right (Just response)) <- runInContext (handleMcpRequest [tool] Nothing Nothing body) context
  let (Just result) = valueField "result" (toJSON response)
  return result

toolWorkspace :: Workspace
toolWorkspace = secondWorkspace {defaultRoleUuid = Nothing} :: Workspace

insertToolWorkspace :: RequestContextM ()
insertToolWorkspace = runInTransaction . void $ insertWorkspace toolWorkspace

failingTool :: RequestContextM Value
failingTool =
  runInTransaction $ do
    void $ insertWorkspace toolWorkspace
    void . runDB $ \connection -> execute_ connection "SELECT * FROM missing_table"
    return $ object []

openTransaction :: RequestContextM ()
openTransaction = void . runDB $ \connection -> execute_ connection "BEGIN"

isConnectionIdle :: Pool.Pool Connection -> IO Bool
isConnectionIdle pool = (== [Only True]) <$> Pool.withResource pool (`query_` "SELECT now() = statement_timestamp()")

createSingleConnectionPool :: RequestContext -> IO (Pool.Pool Connection)
createSingleConnectionPool requestContext = Pool.createPool (connectPostgreSQL (BS.pack requestContext.serverConfig.database.connectionString)) close 1 60 1

withPool :: Pool.Pool Connection -> RequestContext -> RequestContext
withPool pool requestContext = requestContext {dbPool = pool, dbConnection = Nothing} :: RequestContext

backendPid :: Pool.Pool Connection -> IO [Only Int]
backendPid pool = Pool.withResource pool (`query_` "SELECT pg_backend_pid()")
