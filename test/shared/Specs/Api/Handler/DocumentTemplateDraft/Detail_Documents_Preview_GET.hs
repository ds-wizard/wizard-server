module Specs.Api.Handler.DocumentTemplateDraft.Detail_Documents_Preview_GET (
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
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KME
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftData
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.Project.Project
import Shared.Service.Document.Context.DocumentContextService
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/document-template-drafts/{documentTemplateUuid}/documents/preview
-- ------------------------------------------------------------------------
detail_documents_preview_GET :: RequestContext -> SpecWith ((), Application)
detail_documents_preview_GET requestContext =
  describe "GET /api/document-template-drafts/{documentTemplateUuid}/documents/preview" $ do
    test_200 requestContext
    test_202 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/api/document-template-drafts/" ++ U.toString wizardDocumentTemplateDraft.uuid ++ "/documents/preview"

reqUrlPollT documentUuid = BS.pack $ "/api/document-template-drafts/" ++ U.toString wizardDocumentTemplateDraft.uuid ++ "/documents/preview?documentUuid=" ++ U.toString documentUuid

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK (reuses the finished project preview)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      ctx <- runProjectMigrations requestContext
      runInContextIO (insertDocument (projectPreviewDoc tempDocDone ctx)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, TemporaryFileDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.contentType `shouldBe` "text/plain"
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 1
  it "HTTP 200 OK (polls the knowledge model editor preview by documentUuid)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlPollT tempDocDone.uuid
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      _ <- runEditorMigrations requestContext
      runInContextIO (insertDocument (editorPreviewDoc tempDocDone amsterdamKnowledgeModelEditor "sha256:stale")) requestContext
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
  it "HTTP 202 ACCEPTED (queues a project preview)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      ctx <- runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      documentUuid <- assertAccepted response expStatus expHeaders
      -- AND: Compare state in DB with expectation
      doc <- getOneFromDB (findDocumentByUuid documentUuid) requestContext
      liftIO $ doc.durability `shouldBe` TemporallyDocumentDurability
      liftIO $ doc.projectUuid `shouldBe` Just project1.uuid
      liftIO $ doc.knowledgeModelEditorUuid `shouldBe` Nothing
      liftIO $ doc.documentTemplateUuid `shouldBe` wizardDocumentTemplateDraft.uuid
      liftIO $ doc.contextHash `shouldBe` Just ctx.hash
  it "HTTP 202 ACCEPTED (queues a knowledge model editor preview)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      ctx <- runEditorMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      documentUuid <- assertAccepted response expStatus expHeaders
      -- AND: Compare state in DB with expectation
      doc <- getOneFromDB (findDocumentByUuid documentUuid) requestContext
      liftIO $ doc.durability `shouldBe` TemporallyDocumentDurability
      liftIO $ doc.projectUuid `shouldBe` Nothing
      liftIO $ doc.knowledgeModelEditorUuid `shouldBe` Just amsterdamKnowledgeModelEditor.uuid
      liftIO $ doc.name `shouldBe` amsterdamKnowledgeModelEditor.name
      liftIO $ doc.contextHash `shouldBe` Just ctx.hash
  it "HTTP 202 ACCEPTED (a preview of another knowledge model editor is never shared)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      _ <- runEditorMigrations requestContext
      runInContextIO (insertKnowledgeModelEditor amsterdamKnowledgeModelEditorCopy) requestContext
      runInContextIO (updateDraftDataById (editorDraftData amsterdamKnowledgeModelEditorCopy)) requestContext
      (Right ctx) <- runInContextIO (createKnowledgeModelEditorDocumentContext amsterdamKnowledgeModelEditorCopy) requestContext
      runInContextIO (insertDocument (editorPreviewDoc tempDocDone amsterdamKnowledgeModelEditor ctx.hash)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      documentUuid <- assertAccepted response expStatus expHeaders
      liftIO $ documentUuid `shouldNotBe` tempDocDone.uuid
      -- AND: Compare state in DB with expectation
      doc <- getOneFromDB (findDocumentByUuid documentUuid) requestContext
      liftIO $ doc.knowledgeModelEditorUuid `shouldBe` Just amsterdamKnowledgeModelEditorCopy.uuid
      assertCountInDB findDocuments requestContext 2
  it "HTTP 202 ACCEPTED (polling a preview of another editor falls back)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlPollT tempDocDone.uuid
      -- AND: Prepare expectation
      let expStatus = 202
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      _ <- runEditorMigrations requestContext
      runInContextIO (insertKnowledgeModelEditor amsterdamKnowledgeModelEditorCopy) requestContext
      runInContextIO (insertDocument (editorPreviewDoc tempDocDone amsterdamKnowledgeModelEditorCopy "sha256:stale")) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      documentUuid <- assertAccepted response expStatus expHeaders
      liftIO $ documentUuid `shouldNotBe` tempDocDone.uuid
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 2

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
      _ <- runProjectMigrations requestContext
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
    "/api/document-template-drafts/3db4265e-8ba2-433d-97fb-6cc504866bbd/documents/preview"
    reqHeaders
    reqBody
    "document_template_draft_data"
    [("document_template_uuid", "3db4265e-8ba2-433d-97fb-6cc504866bbd")]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
runProjectMigrations requestContext = do
  runInContextIO U.runMigration requestContext
  runInContextIO TML.runMigration requestContext
  runInContextIO PRJ.runMigration requestContext
  runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
  runInContextIO deleteDocuments requestContext
  runInContextIO (insertDraftData wizardDocumentTemplateDraftData) requestContext
  (Right (ctx, _)) <- runInContextIO (createProjectDocumentContext project1 Nothing) requestContext
  return ctx

runEditorMigrations requestContext = do
  runInContextIO U.runMigration requestContext
  runInContextIO TML.runMigration requestContext
  runInContextIO KME.runMigration requestContext
  runInContextIO deleteDocuments requestContext
  runInContextIO (insertDraftData (editorDraftData amsterdamKnowledgeModelEditor)) requestContext
  (Right ctx) <- runInContextIO (createKnowledgeModelEditorDocumentContext amsterdamKnowledgeModelEditor) requestContext
  return ctx

editorDraftData :: KnowledgeModelEditor -> DocumentTemplateDraftData
editorDraftData editor = wizardDocumentTemplateDraftData {projectUuid = Nothing, knowledgeModelEditorUuid = Just editor.uuid}

projectPreviewDoc :: Document -> DocumentContext -> Document
projectPreviewDoc doc ctx = doc {name = project1.name, documentTemplateUuid = wizardDocumentTemplateDraft.uuid, contextHash = Just ctx.hash}

editorPreviewDoc :: Document -> KnowledgeModelEditor -> String -> Document
editorPreviewDoc doc editor contextHash =
  doc
    { name = editor.name
    , projectUuid = Nothing
    , projectEventUuid = Nothing
    , knowledgeModelEditorUuid = Just editor.uuid
    , documentTemplateUuid = wizardDocumentTemplateDraft.uuid
    , contextHash = Just contextHash
    }
