module Specs.Api.Handler.Project.Detail_WS.SetReplySpec where

import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
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
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectUtil
import Shared.Service.Project.Event.ProjectEventMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Websocket

import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Detail_WS.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

setReplySpec requestContext =
  describe "setReply" $ do
    test200 requestContext
    test400 requestContext
    test403 requestContext

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
      -- WHEN:
      write_SetReply c1 (toEventChangeDTO (sre_rQ1Updated' project10Uuid))
      -- THEN:
      read_SetReply c1 (toEventDTO (sre_rQ1Updated' project10Uuid) (Just userAlbert))
      read_SetReply c2 (toEventDTO (sre_rQ1Updated' project10Uuid) (Just userAlbert))
      read_SetReply c3 (toEventDTO (sre_rQ1Updated' project10Uuid) (Just userAlbert))
      nothingWasReceived c4
      nothingWasReceived c5
      nothingWasReceived c6
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4, s5, s6]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test400 requestContext =
  it "WS 400 BAD REQUEST - path doesn't match the knowledge model" $
    -- GIVEN: Prepare database
    do
      insertProjectAndUsers requestContext project10
      -- AND: Prepare expectation
      let invalidPath = createReplyKey [chapter1.uuid, question3.uuid]
      let expError = ValidationError [] (M.singleton "path" [_ERROR_VALIDATION__PROJECT_EVENT_PATH_ABSENCE invalidPath])
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project10.uuid
      -- WHEN: Send setReply
      write_SetReply c1 (SetReplyEventChangeDTO' (srche_rQ2 {path = invalidPath} :: SetReplyEventChangeDTO))
      -- THEN: Read response
      read_Error c1 expError
      nothingWasReceived c2
      nothingWasReceived c3
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test403 requestContext = do
  create_403_no_perm
    "WS 403 FORBIDDEN - no required edit entity permission (Anonymous, VisibleView)"
    requestContext
    project5
    Nothing
    "Edit Project"
  create_403_no_perm
    "WS 403 FORBIDDEN - no required edit entity permission (Non-owner, VisibleView)"
    requestContext
    project5
    (Just reqNonAdminAuthToken)
    "Edit Project"

create_403_no_perm title requestContext project authToken errorMessage =
  it title $
    -- GIVEN: Prepare database
    do
      insertProjectAndUsers requestContext project
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN errorMessage
      -- AND: Connect to websocket
      (c1, s1) <- createConnection requestContext (reqUrlT project.uuid authToken)
      read_SetUserList c1 0
      -- WHEN: Send setReply
      write_SetReply c1 (toEventChangeDTO (sre_rQ1Updated' project.uuid))
      -- THEN: Read response
      read_Error c1 expError
      -- AND: Close sockets
      closeSockets [s1]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
write_SetReply connection replyDto = do
  let reqDto = SetContent_ClientProjectMessageDTO replyDto
  sendMessage connection reqDto

read_SetReply connection expReplyDto = do
  resDto <- receiveDataWithin connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerProjectMessageDTO replyDto))) = eResult
  expReplyDto `shouldBe` replyDto
