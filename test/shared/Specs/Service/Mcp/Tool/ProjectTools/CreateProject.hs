module Specs.Service.Mcp.Tool.ProjectTools.CreateProject (
  create_project,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (create_project)
-- ------------------------------------------------------------------------
create_project :: RequestContext -> SpecWith ((), Application)
create_project requestContext =
  describe "POST /api/mcp (create_project)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments =
  object
    [ "uuid" .= newProjectUuid
    , "name" .= ("Test data" :: String)
    , "knowledgeModelPackageUuid" .= germanyKmPackage.uuid
    , "questionTagUuids" .= [tagDataScience.uuid]
    , "documentTemplateUuid" .= wizardDocumentTemplate.uuid
    , "formatUuid" .= formatJson.uuid
    ]

reqBody = toolCallBody "create_project" reqArguments

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
      liftIO $ jsonField "uuid" structuredContent `shouldBe` toJSON newProjectUuid
      assertToolOutputSchema reqHeaders "create_project" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectInDB requestContext newProject
  it "HTTP 200 OK if the call is repeated (the project created once)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runProjectMigrations requestContext
      firstResponse <- request reqMethod reqUrl reqHeaders reqBody
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ structuredContentOf response `shouldBe` structuredContentOf firstResponse
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectInDB requestContext newProject
  it "HTTP 200 OK if project_template_uuid is given (the project created from the template)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "create_project" (object ["uuid" .= newProjectUuid, "project_template_uuid" .= project1.uuid, "name" .= ("From template" :: String)])
      -- AND: Prepare expectation
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
      liftIO $ (jsonField "uuid" <$> structuredContentOf response) `shouldBe` Just (toJSON newProjectUuid)
      -- AND: Compare state in DB with expectation
      project <- getOneFromDB (findProjectByUuid newProjectUuid) requestContext
      liftIO $ (project.name, project.isTemplate, project.knowledgeModelPackageUuid) `shouldBe` ("From template", False, project1.knowledgeModelPackageUuid)
  it "HTTP 200 OK if the project has no knowledge model package (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "create_project" (object ["name" .= ("Test data" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__MISSING_ARGUMENTS "project" ["knowledgeModelPackageUuid"])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION
  createToolNoPermissionTest requestContext reqBody _PROJECTS_CREATE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
