module Specs.Service.Mcp.Tool.DocumentTemplatePreviewTools.Common where

import Control.Monad (void)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection, begin, connectPostgreSQL)
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as Project
import qualified Shared.Database.Migration.Development.User.UserMigration as User
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftData
import Shared.Model.Project.Project
import Shared.S3.Document.DocumentS3
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runPreviewMigrations requestContext = do
  runInContextIO User.runMigration requestContext
  runInContextIO DocumentTemplate.runMigration requestContext
  runInContextIO Project.runMigration requestContext
  runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
  runInContextIO deleteDocuments requestContext
  runInContextIO (insertDraftData wizardDocumentTemplateDraftData) requestContext

insertPendingPreviews requestContext count = do
  now <- liftIO getCurrentTime
  traverse_ (\index -> runInContextIO (insertDocument (pendingPreview now index)) requestContext) [1 .. count]

previewArguments :: [(K.Key, Value)] -> Value
previewArguments arguments = object (("document_template_uuid" .= wizardDocumentTemplateDraft.uuid) : arguments)

previewProgressBody :: [(K.Key, Value)] -> BSL.ByteString
previewProgressBody arguments =
  rpcBody
    "tools/call"
    ( object
        [ "name" .= ("preview_document_template" :: String)
        , "arguments" .= previewArguments arguments
        , "_meta" .= object ["progressToken" .= ("preview-1" :: String)]
        ]
    )

emptyDraftData :: DocumentTemplateDraftData
emptyDraftData = wizardDocumentTemplateDraftData {projectUuid = Nothing, formatUuid = Nothing}

queuedPreview :: Document
queuedPreview =
  tempDocDone
    { state = QueuedDocumentState
    , projectUuid = Just project1.uuid
    , documentTemplateUuid = wizardDocumentTemplateDraft.uuid
    , formatUuid = formatJson.uuid
    , fileName = Nothing
    , contentType = Nothing
    , fileSize = Nothing
    , workerLog = Nothing
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , knowledgeModelEditorUuid = Nothing
    }

pendingPreview :: UTCTime -> Int -> Document
pendingPreview now index =
  tempDocDone
    { uuid = u' ("0c7f7e1e-0000-4000-8000-00000000000" ++ show index)
    , state = if even index then InProgressDocumentState else QueuedDocumentState
    , createdAt = now
    }

workerLog :: String
workerLog =
  unlines
    [ "[INFO] Preparing the document context"
    , "[ERROR] Failed to load the asset logo.png"
    , "[INFO] Rendering the template"
    , "Traceback (most recent call last):"
    , "  File \"/app/document_worker/templates/steps.py\", line 88, in execute"
    , "    result = template.render(ctx)"
    , "  File \"src/default.html.j2\", line 12, in top-level template code"
    , "    {{ ctx.foo.bar }}"
    , "jinja2.exceptions.UndefinedError: 'dict object' has no attribute 'foo'"
    , "[INFO] Job finished"
    ]

trimmedWorkerLog :: String
trimmedWorkerLog =
  unlines
    [ "  File \"src/default.html.j2\", line 12, in top-level template code"
    , "    {{ ctx.foo.bar }}"
    , "jinja2.exceptions.UndefinedError: 'dict object' has no attribute 'foo'"
    ]

-- --------------------------------
-- HELPERS
-- --------------------------------
queuePreview :: RequestContext -> WaiSession () U.UUID
queuePreview requestContext = do
  _ <- request methodPost "/api/mcp" [reqAuthHeader, reqCtHeader] (toolCallBody "preview_document_template" (previewArguments []))
  doc <- findPreviewDocument requestContext
  return doc.uuid

findPreviewDocument :: RequestContext -> WaiSession () Document
findPreviewDocument requestContext = do
  [doc] <- getOneFromDB findDocuments requestContext
  return doc

finishPreview :: RequestContext -> U.UUID -> String -> String -> WaiSession () ()
finishPreview requestContext documentUuid contentType content = do
  replaceDocument requestContext documentUuid $ \doc ->
    doc {state = DoneDocumentState, fileName = Just "preview.json", contentType = Just contentType, fileSize = Just (fromIntegral (length content))}
  void $ runInContextIO (putDocumentContent documentUuid (BS.pack content)) requestContext

failPreview :: RequestContext -> U.UUID -> WaiSession () ()
failPreview requestContext documentUuid =
  replaceDocument requestContext documentUuid $ \doc -> doc {state = ErrorDocumentState, workerLog = Just workerLog}

replaceDocument :: RequestContext -> U.UUID -> (Document -> Document) -> WaiSession () ()
replaceDocument requestContext documentUuid change = do
  doc <- getOneFromDB (findDocumentByUuid documentUuid) requestContext
  void $ runInContextIO (deleteDocumentByUuid documentUuid) requestContext
  void $ runInContextIO (insertDocument (change doc)) requestContext

startConcurrentPreview :: RequestContext -> Document -> IO Connection
startConcurrentPreview requestContext doc = do
  connection <- connectPostgreSQL (BS.pack requestContext.serverConfig.database.connectionString)
  begin connection
  let previewContext = requestContext {dbConnection = Just connection} :: RequestContext
  (Right _) <- runInContext (lockTemporaryDocumentsOfCurrentTenant >> insertDocument doc) previewContext
  return connection

textOf :: Value -> String
textOf (String text) = T.unpack text
textOf _ = ""

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertPreviewDocumentInDB :: RequestContext -> Document -> WaiSession () ()
assertPreviewDocumentInDB requestContext expDocument = do
  doc <- findPreviewDocument requestContext
  liftIO $
    doc
      `shouldBe` expDocument
        { uuid = doc.uuid
        , name = doc.name
        , projectEventUuid = doc.projectEventUuid
        , language = doc.language
        , createdBy = doc.createdBy
        , tenantUuid = doc.tenantUuid
        , createdAt = doc.createdAt
        , workspaceUuid = doc.workspaceUuid
        , contextHash = doc.contextHash
        }
