module Specs.Service.Project.Comment.ProjectCommentServiceSpec where

import Data.Aeson (Value (..), eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.UUID as U
import Test.Hspec

import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Shared.Database.Migration.Development.Tenant.Data.Tenants (defaultTenant)
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.Project.Project
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Service.Project.Comment.ProjectCommentService
import WizardServer.Model.Context.RequestContext

import Specs.Common

projectCommentServiceSpec requestContext =
  describe "Project Comment Service" $
    it "sendNotificationToNewAssignees sends the workspace and the tenant client URL to the mailer" $
      -- GIVEN:
      do
        let context = cloudRequestContext requestContext
        runInContextIO U_Migration.runMigration context
        runInContextIO TML_Migration.runMigration context
        runInContextIO PRJ_Migration.runMigration context
        runInContextIO (updateProjectCommentThreadAssignee cmtQ1_t1.uuid (Just userNikola.uuid) (Just userAlbert.uuid)) context
        -- WHEN:
        (Right ()) <- runInContext sendNotificationToNewAssignees context
        -- THEN:
        (Right commands) <- runInContext (findPersistentCommands :: RequestContextM [PersistentCommand U.UUID]) context
        let mailCommands = filter (\command -> command.component == "mailer") commands
        length mailCommands `shouldBe` 1
        let parameters = mailParameters (head mailCommands)
        ((firstNotification =<< parameters) >>= KM.lookup "workspaceUuid") `shouldBe` Just (String . T.pack . U.toString $ project1.workspaceUuid)
        (parameters >>= KM.lookup "clientUrl") `shouldBe` Just (String . T.pack $ defaultTenant.clientUrl)

cloudRequestContext :: RequestContext -> RequestContext
cloudRequestContext requestContext =
  let config = requestContext.serverConfig
      cloudConfig = config.cloud {enabled = True} :: ServerConfigCloud
   in requestContext {serverConfig = (config {cloud = cloudConfig}) :: ServerConfig} :: RequestContext

mailParameters :: PersistentCommand U.UUID -> Maybe (KM.KeyMap Value)
mailParameters command =
  case eitherDecode (TLE.encodeUtf8 (TL.pack command.body)) of
    Right (Object body) ->
      case KM.lookup "parameters" body of
        Just (Object parameters) -> Just parameters
        _ -> Nothing
    _ -> Nothing

firstNotification :: KM.KeyMap Value -> Maybe (KM.KeyMap Value)
firstNotification parameters =
  case KM.lookup "notifications" parameters of
    Just (Array notifications) ->
      case toList notifications of
        (Object notification : _) -> Just notification
        _ -> Nothing
    _ -> Nothing
