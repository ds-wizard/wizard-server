module Specs.Api.Handler.Project.Detail_Documents_Preview_GET (
  detail_documents_preview_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectFileDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectFiles
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Service.Document.Context.DocumentContextService
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/projects/{projectUuid}/documents/preview
-- ------------------------------------------------------------------------
detail_documents_preview_GET :: RequestContext -> SpecWith ((), Application)
detail_documents_preview_GET requestContext =
  describe "GET /api/projects/{projectUuid}/documents/preview" $ do
    test_200 requestContext
    test_202 requestContext
    test_400 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT projectUuid = BS.pack $ "/api/projects/" ++ U.toString projectUuid ++ "/documents/preview"

reqUrlPollT projectUuid documentUuid = BS.pack $ "/api/projects/" ++ U.toString projectUuid ++ "/documents/preview?documentUuid=" ++ U.toString documentUuid

reqUrl = reqUrlT project1.uuid

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK (reuses the finished preview)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      ctx <- runMigrations requestContext
      runInContextIO (insertDocument (previewDoc tempDocDone ctx)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, TemporaryFileDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.contentType `shouldBe` "text/plain"
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 1
  it "HTTP 200 OK (polls the preview by documentUuid)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlPollT project1.uuid staleDoneDoc.uuid
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      _ <- runMigrations requestContext
      runInContextIO (insertDocument staleDoneDoc) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, _) = destructResponse response :: (Int, ResponseHeaders, TemporaryFileDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_202 requestContext = do
  it "HTTP 202 ACCEPTED (queues a new preview)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      ctx <- runMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      documentUuid <- assertAccepted response expStatus expHeaders
      -- AND: Compare state in DB with expectation
      doc <- getOneFromDB (findDocumentByUuid documentUuid) requestContext
      liftIO $ doc.durability `shouldBe` TemporallyDocumentDurability
      liftIO $ doc.projectUuid `shouldBe` Just project1.uuid
      liftIO $ doc.contextHash `shouldBe` Just ctx.hash
      liftIO $ doc.name `shouldBe` project1.name
  it "HTTP 202 ACCEPTED (polling a queued preview returns it)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlPollT project1.uuid tempDocQueued.uuid
      -- AND: Prepare expectation
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      ctx <- runMigrations requestContext
      runInContextIO (insertDocument (previewDoc tempDocQueued ctx)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      documentUuid <- assertAccepted response expStatus expHeaders
      liftIO $ documentUuid `shouldBe` tempDocQueued.uuid
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 1
  it "HTTP 202 ACCEPTED (polling a preview of another project falls back)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlPollT project1.uuid staleDoneDoc.uuid
      -- AND: Prepare expectation
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      _ <- runMigrations requestContext
      runInContextIO (insertDocument (staleDoneDoc {projectUuid = Just project2.uuid} :: Document)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      documentUuid <- assertAccepted response expStatus expHeaders
      liftIO $ documentUuid `shouldNotBe` staleDoneDoc.uuid
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 2
  it "HTTP 202 ACCEPTED (polling a persistent document falls back)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlPollT project1.uuid doc1.uuid
      -- AND: Prepare expectation
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      ctx <- runMigrations requestContext
      runInContextIO (insertDocument (previewDoc doc1 ctx)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      documentUuid <- assertAccepted response expStatus expHeaders
      liftIO $ documentUuid `shouldNotBe` doc1.uuid
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 2
  it "HTTP 202 ACCEPTED (polling a missing preview falls back)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlPollT project1.uuid (u' "4c9d2a1e-7b3f-4e8a-9d6c-1f2e3a4b5c6d")
      -- AND: Prepare expectation
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      _ <- runMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      _ <- assertAccepted response expStatus expHeaders
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 1
  it "HTTP 202 ACCEPTED (a label change invalidates the preview)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      ctx <- runMigrations requestContext
      runInContextIO (insertDocument (previewDoc tempDocDone ctx)) requestContext
      runInContextIO (insertProjectEvent (slble_rQ2' project1.uuid)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      _ <- assertAccepted response expStatus expHeaders
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 2
  it "HTTP 202 ACCEPTED (a file change invalidates the preview)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      ctx <- runMigrations requestContext
      runInContextIO (insertDocument (previewDoc tempDocDone ctx)) requestContext
      runInContextIO (insertProjectFile projectFile) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      _ <- assertAccepted response expStatus expHeaders
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 2
  it "HTTP 202 ACCEPTED (a preview with another name is not reused)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      ctx <- runMigrations requestContext
      runInContextIO (insertDocument ((previewDoc tempDocDone ctx) {name = "Another name"} :: Document)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      _ <- assertAccepted response expStatus expHeaders
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 2
  it "HTTP 202 ACCEPTED (a persistent document with the same hash is not reused)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      ctx <- runMigrations requestContext
      runInContextIO (insertDocument (previewDoc doc1 ctx)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      documentUuid <- assertAccepted response expStatus expHeaders
      liftIO $ documentUuid `shouldNotBe` doc1.uuid
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST (the preview failed)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = SystemLogError (_ERROR_SERVICE_PROJECT__UNABLE_TO_GENERATE_DOCUMENT_PREVIEW tempDocError.workerLog)
      let expBody = encode expDto
      -- AND: Run migrations
      ctx <- runMigrations requestContext
      runInContextIO (insertDocument (previewDoc tempDocError ctx)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext =
  it "HTTP 403 FORBIDDEN (Non-Owner, Private)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Project"
      let expBody = encode expDto
      -- AND: Run migrations
      _ <- runMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl [reqNonAdminAuthHeader] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/documents/preview"
    reqHeaders
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
runMigrations requestContext = do
  runInContextIO U.runMigration requestContext
  runInContextIO TML.runMigration requestContext
  runInContextIO PRJ.runMigration requestContext
  runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
  runInContextIO deleteDocuments requestContext
  (Right (ctx, _)) <- runInContextIO (createProjectDocumentContext project1 Nothing) requestContext
  return ctx

previewDoc :: Document -> DocumentContext -> Document
previewDoc doc ctx = doc {name = project1.name, contextHash = Just ctx.hash}

staleDoneDoc :: Document
staleDoneDoc = tempDocDone {name = project1.name, contextHash = Just "sha256:stale"}
