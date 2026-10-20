module Specs.Service.Mcp.Tool.UserTools.Common where

import Data.Aeson (Value (..))
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.User.UserDAO
import Shared.Model.User.User
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
embeddedField :: String -> String -> Value -> [Value]
embeddedField collection field content =
  case valueField collection =<< valueField "_embedded" content of
    Just (Array entities) -> mapMaybe (valueField field) (V.toList entities)
    _ -> []

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfUserByEmailInDB :: RequestContext -> String -> (User -> User) -> WaiSession () ()
assertExistenceOfUserByEmailInDB requestContext email expUser = do
  user <- getOneFromDB (findUserByEmail email) requestContext
  liftIO $ user `shouldBe` expUser user

assertAbsenceOfUserByEmailInDB :: RequestContext -> String -> WaiSession () ()
assertAbsenceOfUserByEmailInDB requestContext email = do
  mUser <- getOneFromDB (findUserByEmail' email) requestContext
  liftIO $ fmap (.uuid) mUser `shouldBe` Nothing
