module Specs.Api.Handler.Project.Detail_WS.EventBatchSpec where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (isRight)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status (badRequest400, created201)
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.Project.ProjectContentChangeDTO
import Shared.Api.Resource.Project.ProjectContentChangeJM ()
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Project.Data.ProjectEventBatches
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Http.HttpRequest
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectUtil
import Shared.Service.Project.Event.ProjectEventMapper
import Shared.Util.Date

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Detail_WS.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

eventBatchSpec requestContext =
  describe "eventBatch" $ do
    test200 requestContext
    test400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 requestContext = do
  create_test_200 "WS 200 OK" requestContext [SetReplyEventChangeDTO' srche_rQ1, SetReplyEventChangeDTO' srche_rQ2]
  create_test_200
    "WS 200 OK - a large event through the websocket outbox"
    requestContext
    [AddCommentEventChangeDTO' acche_rQ2_t1_1 {text = replicate 10000 'x'}]

create_test_200 title requestContext events =
  it title $
    -- GIVEN: Prepare database
    do
      insertProjectsAndUsers requestContext
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project10.uuid
      ((c4, s4), (c5, s5), (c6, s6)) <- connectTestWebsocketUsers requestContext project7.uuid
      -- AND: Prepare expectation
      let expEvents = fmap (\event -> toEventDTO' event (Just userAlbertSuggestion) (dt' 2018 1 21)) events
      -- WHEN:
      response <- runSimpleRequest requestContext (batchRequest events)
      -- THEN:
      isRight response `shouldBe` True
      let (Right r) = response
      HC.responseStatus r `shouldBe` created201
      -- AND:
      traverse_ (\connection -> traverse_ (read_SetContent connection) expEvents) [c1, c2, c3]
      traverse_ nothingWasReceived [c1, c2, c3, c4, c5, c6]
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4, s5, s6]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test400 requestContext =
  it "WS 400 BAD REQUEST - a rejected batch is not broadcast" $
    -- GIVEN: Prepare database
    do
      insertProjectsAndUsers requestContext
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project10.uuid
      -- AND: Prepare request
      let invalidPath = createReplyKey [chapter1.uuid, question3.uuid]
      let events = [SetReplyEventChangeDTO' srche_rQ1, SetReplyEventChangeDTO' (srche_rQ2 {path = invalidPath} :: SetReplyEventChangeDTO)]
      -- WHEN:
      response <- runSimpleRequest requestContext (batchRequest events)
      -- THEN:
      isRight response `shouldBe` True
      let (Right r) = response
      HC.responseStatus r `shouldBe` badRequest400
      -- AND:
      traverse_ nothingWasReceived [c1, c2, c3]
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
insertProjectsAndUsers requestContext = do
  runInContext U.runMigration requestContext
  runInContextIO TML_Migration.runMigration requestContext
  runInContextIO (insertPackage germanyKmPackage) requestContext
  runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
  runInContextIO (insertProject project10) requestContext
  runInContextIO (insertProject project7) requestContext

batchRequest events =
  HttpRequest
    { requestMethod = "POST"
    , requestUrl = "/api/projects/" ++ U.toString project10.uuid ++ "/events/batch"
    , requestHeaders = M.fromList [("Authorization", "Bearer " ++ reqAuthToken), ("Content-Type", "application/json")]
    , requestBody = BSL.toStrict . encode $ ProjectContentChangeDTO {events = events}
    , multipart = Nothing
    }

read_SetContent connection expEventDto = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerProjectMessageDTO eventDto))) = eResult
  expEventDto `shouldBe` eventDto
