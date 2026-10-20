module Specs.Service.Mcp.Tool.ProjectFileTools.UploadProjectFile (
  upload_project_file,
) where

import Data.Aeson
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Project.ProjectFileDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Public
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.ProjectUtil
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (upload_project_file)
-- ------------------------------------------------------------------------
upload_project_file :: RequestContext -> SpecWith ((), Application)
upload_project_file requestContext =
  describe "POST /api/mcp (upload_project_file)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["path" .= filePath, "file_name" .= "hello.txt", "content_type" .= "text/plain", "content" .= "aGVsbG8="]

reqBody = toolCallBody "upload_project_file" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ jsonField "fileName" structuredContent `shouldBe` String "hello.txt"
      assertToolOutputSchema reqHeaders "upload_project_file" structuredContent
      -- AND: Compare state in DB with expectation
      files <- getOneFromDB (findProjectFilesByProject project1.uuid) requestContext
      liftIO $ fmap (\file -> (file.fileName, file.fileSize)) files `shouldBe` [("hello.txt", 5)]
      replies <- findProjectReplies requestContext
      liftIO $ fmap (.value) (M.lookup filePath replies) `shouldBe` fmap (FileReply . (.uuid)) (headMaybe files)
  createIsaacToolErrorTest "the user only views the project" requestContext (runProjectMigrations requestContext) viewerPermissions reqBody (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION

filePath :: String
filePath = createReplyKey [chapter3.uuid, question14.uuid]

headMaybe :: [a] -> Maybe a
headMaybe (x : _) = Just x
headMaybe [] = Nothing
