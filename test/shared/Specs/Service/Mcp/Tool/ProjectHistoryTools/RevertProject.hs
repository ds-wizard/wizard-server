module Specs.Service.Mcp.Tool.ProjectHistoryTools.RevertProject (
  revert_project,
) where

import Data.Aeson
import qualified Data.List as L
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Public
import Shared.Model.Common.Lens
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Event.ProjectEventLenses ()
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (revert_project)
-- ------------------------------------------------------------------------
revert_project :: RequestContext -> SpecWith ((), Application)
revert_project requestContext =
  describe "POST /api/mcp (revert_project)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["event_uuid" .= (sre_rQ2 project1.uuid).uuid]

reqBody = toolCallBody "revert_project" reqArguments

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
      liftIO $ fmap (L.isInfixOf "13 later events, 1 versions (Version 1)") (stringField "summary" structuredContent) `shouldBe` Just True
      liftIO $ isJust (stringField "confirmation_token" structuredContent) `shouldBe` True
      -- AND: Compare state in DB with expectation
      events <- getOneFromDB (findProjectEventsByProjectUuid project1.uuid) requestContext
      liftIO $ length events `shouldBe` length (fEvents project1.uuid)
  it "HTTP 200 OK if the confirmation token is given (the project reverted)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object [])
      -- AND: Run migrations
      runProjectMigrations requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "revert_project" (projectArguments ["event_uuid" .= (sre_rQ2 project1.uuid).uuid, "confirmation_token" .= token])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      events <- getOneFromDB (findProjectEventsByProjectUuid project1.uuid) requestContext
      liftIO $ fmap getUuid events `shouldBe` fmap getUuid [sre_rQ1' project1.uuid, sre_rQ2' project1.uuid]
      versions <- getOneFromDB (findProjectVersionsByProjectUuid project1.uuid) requestContext
      liftIO $ versions `shouldBe` []
  createIsaacToolErrorTest "the user only edits the project" requestContext (runProjectMigrations requestContext) editorPermissions reqBody (_ERROR_VALIDATION__FORBIDDEN "Administrate Project")
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
