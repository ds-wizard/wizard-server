module Specs.Service.Mcp.Tool.ProjectTools.Common where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai.Test (SResponse)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as Project
import qualified Shared.Database.Migration.Development.User.UserMigration as User
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectContent
import Shared.Model.Project.ProjectReply
import Shared.Service.Project.Compiler.ProjectCompilerService
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runProjectMigrations requestContext = do
  runInContextIO User.runMigration requestContext
  runInContextIO DocumentTemplate.runMigration requestContext
  runInContextIO Project.runMigration requestContext
  runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext

projectArguments :: [(K.Key, Value)] -> Value
projectArguments arguments = object (("project_uuid" .= project1.uuid) : arguments)

manyReplyEvents :: [ProjectEvent]
manyReplyEvents =
  [ SetReplyEvent' (sre_rQ1Updated project1.uuid) {uuid = U.fromWords 0 0 0 (fromIntegral index), path = "many." ++ show index, value = StringReply (replicate 300 'x')}
  | index <- [1 .. 200 :: Int]
  ]

newProjectUuid :: U.UUID
newProjectUuid = u' "4b6d8f0a-2c4e-4a6b-8d0f-3e5a7c9b1d24"

newProject :: Project
newProject =
  project1
    { uuid = newProjectUuid
    , name = "Test data"
    , description = Nothing
    , selectedQuestionTagUuids = [tagDataScience.uuid]
    , projectTags = []
    , permissions = [project1AlbertEditProjectPerm {projectUuid = newProjectUuid}]
    , isTemplate = False
    , squashed = True
    }

newItemUuid :: U.UUID
newItemUuid = u' "5c7e9a1b-3d5f-4b7c-9e1a-4f6b8d0c2e35"

-- --------------------------------
-- HELPERS
-- --------------------------------
findProjectReplies :: RequestContext -> WaiSession () (M.Map String Reply)
findProjectReplies requestContext = do
  events <- getOneFromDB (findProjectEventListsByProjectUuid project1.uuid) requestContext
  return (compileProjectEvents events).replies

requestQuestionnairePages :: Value -> Int -> WaiSession st [SResponse]
requestQuestionnairePages arguments remaining = do
  response <- request methodPost "/api/mcp" [reqAuthHeader, reqCtHeader] (toolCallBody "get_project_questionnaire" arguments)
  case (valueField "cursor" =<< structuredContentOf response, arguments) of
    (Just cursor, Object o) | remaining > 1 -> (response :) <$> requestQuestionnairePages (Object (KM.insert "cursor" cursor o)) (remaining - 1)
    _ -> return [response]

pageReplies :: SResponse -> M.Map String Value
pageReplies response =
  case fromJSON . jsonField "thirdPartyContent" . jsonField "replies" <$> structuredContentOf response of
    Just (Success replies) -> replies
    _ -> M.empty

projectUuidsOf :: SResponse -> [Maybe Value]
projectUuidsOf response =
  case valueField "projects" =<< valueField "_embedded" =<< structuredContentOf response of
    Just (Array projects) -> fmap (valueField "uuid") (V.toList projects)
    _ -> []

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfProjectInDB :: RequestContext -> Project -> WaiSession () ()
assertExistenceOfProjectInDB requestContext expProject = do
  project <- getOneFromDB (findProjectByUuid expProject.uuid) requestContext
  liftIO $ project `shouldBe` expProject

assertAbsenceOfProjectInDB :: RequestContext -> U.UUID -> WaiSession () ()
assertAbsenceOfProjectInDB requestContext projectUuid = do
  mProject <- getOneFromDB (findProjectByUuid' projectUuid) requestContext
  liftIO $ mProject `shouldBe` Nothing

assertProjectRepliesInDB :: RequestContext -> M.Map String ReplyValue -> WaiSession () ()
assertProjectRepliesInDB requestContext expReplies = do
  replies <- findProjectReplies requestContext
  liftIO $ fmap (.value) replies `shouldBe` expReplies
