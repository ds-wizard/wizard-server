module Specs.Api.Handler.Project.Detail_WS.SetProjectSpec where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (isRight)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status (ok200)
import Network.WebSockets
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Http.HttpRequest
import Shared.Model.Project.Project

import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Detail_WS.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

setProjectSpec requestContext =
  describe "setProjectSpec" $
    test200 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 requestContext =
  it "WS 200 OK" $
    -- GIVEN: Prepare database
    do
      runInContext U.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO (insertProject project10) requestContext
      runInContextIO (insertProject project7) requestContext
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project10.uuid
      ((c4, s4), (c5, s5), (c6, s6)) <- connectTestWebsocketUsers requestContext project7.uuid
      -- AND: Prepare request
      let request =
            HttpRequest
              { requestMethod = "PUT"
              , requestUrl = "/wizard-api/projects/" ++ U.toString project10.uuid ++ "/settings"
              , requestHeaders = M.fromList [("Authorization", "Bearer " ++ reqAuthToken), ("Content-Type", "application/json")]
              , requestBody = BSL.toStrict . encode $ project10EditedSettingsChange
              , multipart = Nothing
              }
      -- AND: Prepare expectation
      let expStatus = 200
      -- WHEN:
      response <- runSimpleRequest requestContext request
      -- THEN:
      isRight response `shouldBe` True
      let (Right r) = response
      HC.responseStatus r `shouldBe` ok200
      -- AND:
      read_SetProject c1 project10EditedWs
      read_SetProject c2 project10EditedWs
      read_SetProject c3 project10EditedWs
      nothingWasReceived c4
      nothingWasReceived c5
      nothingWasReceived c6
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4, s5, s6]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
read_SetProject connection expProjectChangeDto = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO)
  let (Right (Success_ServerActionDTO (SetProject_ServerProjectMessageDTO projectChangeDto))) = eResult
  expProjectChangeDto `shouldBe` projectChangeDto
