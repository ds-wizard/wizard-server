module Specs.Service.Mcp.Tool.OperationTools.Common where

import Data.Aeson
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.Migration.Development.PersistentCommand.Data.WizardPersistentCommands
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Util.Uuid
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
executeArguments :: [(Key, Value)]
executeArguments =
  [ "section" .= ("Cache" :: String)
  , "operation" .= ("Purge All Caches" :: String)
  ]

failedCommand :: PersistentCommand U.UUID
failedCommand = command1 {uuid = u' "7da04f6c-9e3b-4c1a-b5f8-2a4b6c8e0d35", state = ErrorPersistentCommandState, component = "mailer"}

doneCommand :: PersistentCommand U.UUID
doneCommand = command1 {uuid = u' "8eb15a7d-af4c-4d2b-a6a9-3b5c7d9f1e46"}

sectionNames :: Value -> [Value]
sectionNames (Array sections) = foldr (\section names -> maybe names (: names) (valueField "name" section)) [] sections
sectionNames _ = []

-- --------------------------------
-- MIGRATIONS
-- --------------------------------
insertCommand :: RequestContext -> PersistentCommand U.UUID -> WaiSession () ()
insertCommand requestContext command = do
  _ <- runInContextIO (insertPersistentCommand command) requestContext
  return ()

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfPersistentCommandInDB :: RequestContext -> PersistentCommand U.UUID -> WaiSession () ()
assertExistenceOfPersistentCommandInDB requestContext expCommand = do
  command <- getOneFromDB (findPersistentCommandByUuid expCommand.uuid) requestContext
  liftIO $ command `shouldBe` expCommand {createdAt = command.createdAt, updatedAt = command.updatedAt}
