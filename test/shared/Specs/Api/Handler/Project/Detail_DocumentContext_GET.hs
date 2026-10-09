module Specs.Api.Handler.Project.Detail_DocumentContext_GET (
  detail_documentContext_GET,
) where

import Data.Aeson (Value (..), encode, toJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as USR
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.User.Public
import Shared.Model.Document.DocumentContext
import Shared.Model.Document.DocumentContextJM ()
import Shared.Model.Error.Error
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Project
import Shared.Model.Workspace.Workspace
import Shared.Service.Document.Context.DocumentContextHash
import Shared.Service.Document.Context.DocumentContextService
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/projects/{projectUuid}/document-context
-- ------------------------------------------------------------------------
detail_documentContext_GET :: RequestContext -> SpecWith ((), Application)
detail_documentContext_GET requestContext =
  describe "GET /api/projects/{projectUuid}/document-context" $ do
    test_200 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT projectUuid query = BS.pack $ "/api/projects/" ++ U.toString projectUuid ++ "/document-context" ++ query

reqHeadersT authHeader = reqCtHeader : authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK (Owner, Private)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project1.uuid ""
      let reqHeaders = reqHeadersT [reqAuthHeader]
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runMigrations requestContext
      expCtx <- buildContext requestContext project1 Nothing
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      result <- destructResponse' response
      let (status, headers, resBody) = result :: (Int, ResponseHeaders, Value)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      assertEtagHeaders headers (hashOf resBody)
      liftIO $ resBody `shouldBe` toJSON expCtx
      liftIO $ hashOf resBody `shouldBe` computeContextValueHash resBody
      liftIO $ jsonKeys resBody `shouldBe` contextKeys
      liftIO $ jsonField "metamodelVersion" resBody `shouldBe` String "19.0"
      liftIO $ jsonField "project" resBody `shouldNotBe` Null
      liftIO $ jsonField "knowledgeModelEditor" resBody `shouldBe` Null
      liftIO $ jsonField "document" resBody `shouldBe` Null
      liftIO $ jsonKeys (jsonField "config" resBody) `shouldBe` L.sort ["clientUrl", "appTitle", "primaryColor", "logoUrl"]
      liftIO $ jsonKeysDeep (jsonField "knowledgeModelPackage" resBody) `shouldNotContain` ["organizationId"]
      liftIO $ jsonKeysDeep (jsonField "knowledgeModelPackage" resBody) `shouldNotContain` ["kmId"]
      liftIO $ jsonKeysDeep (jsonField "knowledgeModelPackage" resBody) `shouldNotContain` ["remoteLatestVersion"]
      liftIO $ jsonKeys (jsonField "report" . jsonField "questionnaire" $ resBody) `shouldNotContain` ["uuid"]
      liftIO $ jsonKeys (jsonField "report" . jsonField "questionnaire" $ resBody) `shouldNotContain` ["createdAt"]
      liftIO $ jsonKeys (jsonField "report" . jsonField "questionnaire" $ resBody) `shouldNotContain` ["updatedAt"]
  it "HTTP 200 OK (Owner, Private, eventUuid)" $
    -- GIVEN: Prepare request
    do
      let eventUuid = (sre_rQ2 project1.uuid).uuid
      let reqUrl = reqUrlT project1.uuid ("?eventUuid=" ++ U.toString eventUuid)
      let reqHeaders = reqHeadersT [reqAuthHeader]
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runMigrations requestContext
      expCtx <- buildContext requestContext project1 (Just eventUuid)
      latestCtx <- buildContext requestContext project1 Nothing
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      result <- destructResponse' response
      let (status, headers, resBody) = result :: (Int, ResponseHeaders, Value)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      assertEtagHeaders headers (hashOf resBody)
      liftIO $ resBody `shouldBe` toJSON expCtx
      liftIO $ hashOf resBody `shouldNotBe` latestCtx.hash
      let questionnaire = jsonField "questionnaire" resBody
      liftIO $ jsonKeys (jsonField "replies" questionnaire) `shouldBe` L.sort [fst rQ1, fst rQ2]
      liftIO $ jsonField "labels" questionnaire `shouldBe` Object mempty
      liftIO $ jsonField "phaseUuid" questionnaire `shouldBe` Null
      liftIO $ jsonField "versionUuid" questionnaire `shouldBe` Null
      liftIO $ jsonField "versions" questionnaire `shouldBe` Array mempty
      liftIO $ jsonField "project" resBody `shouldBe` jsonField "project" (toJSON latestCtx)
      liftIO $ jsonField "files" questionnaire `shouldBe` jsonField "files" (jsonField "questionnaire" (toJSON latestCtx))
  it "HTTP 200 OK (Anonymous, Private, Sharing)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project5.uuid ""
      let reqHeaders = reqHeadersT []
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runMigrations requestContext
      runInContextIO (insertProject project5) requestContext
      runInContextIO (insertProjectEvents project5Events) requestContext
      expCtx <- buildContext requestContext project5 Nothing
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      result <- destructResponse' response
      let (status, headers, resBody) = result :: (Int, ResponseHeaders, Value)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      assertEtagHeaders headers (hashOf resBody)
      liftIO $ resBody `shouldBe` toJSON expCtx
  it "HTTP 200 OK (Owner, Private, w ignored)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project1.uuid ("?w=" ++ U.toString secondWorkspace.uuid)
      let reqHeaders = reqHeadersT [reqAuthHeader]
      -- AND: Prepare expectation
      let expStatus = 200
      -- AND: Run migrations
      runMigrations requestContext
      expCtx <- buildContext requestContext project1 Nothing
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      result <- destructResponse' response
      let (status, headers, resBody) = result :: (Int, ResponseHeaders, Value)
      assertResStatus status expStatus
      assertEtagHeaders headers expCtx.hash
      liftIO $ resBody `shouldBe` toJSON expCtx

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    requestContext
    project1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "View Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Private)"
    requestContext
    project1
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView, Restricted)"
    requestContext
    project2
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title requestContext project authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid ""
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext = do
  createNotFoundTest'
    reqMethod
    (reqUrlT (u' "f08ead5f-746d-411b-aee6-77ea3d24016a") "")
    (reqHeadersT [reqAuthHeader])
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
  create_test_404_event "HTTP 404 NOT FOUND - unknown eventUuid" requestContext (u' "e4f59b09-4b6a-4d4f-9b19-3d8c6f1a2b3c")
  create_test_404_event "HTTP 404 NOT FOUND - eventUuid of another project" requestContext (sre_rQ2 project2.uuid).uuid

create_test_404_event title requestContext eventUuid =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project1.uuid ("?eventUuid=" ++ U.toString eventUuid)
      let reqHeaders = reqHeadersT [reqAuthHeader]
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND "project_event" [("uuid", U.toString eventUuid)])
      let expBody = encode expDto
      -- AND: Run migrations
      runMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
runMigrations requestContext = do
  runInContextIO USR.runMigration requestContext
  runInContextIO TML.runMigration requestContext
  runInContextIO PRJ.runMigration requestContext
  runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext

buildContext requestContext project mEventUuid = do
  (Right (ctx, _)) <- runInContextIO (createProjectDocumentContext project mEventUuid) requestContext
  return ctx

hashOf = jsonString . jsonField "hash"
