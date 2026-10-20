module Specs.Service.Mcp.Tool.DocumentTools.Common where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.UUID as U
import qualified Data.Vector as V
import Network.Wai.Test (SResponse)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import qualified Shared.Database.Migration.Development.Document.DocumentMigration as Document
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as Project
import qualified Shared.Database.Migration.Development.User.UserMigration as User
import Shared.Model.Document.Document
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runDocumentMigrations requestContext = do
  runInContextIO User.runMigration requestContext
  runInContextIO DocumentTemplate.runMigration requestContext
  runInContextIO Project.runMigration requestContext
  runInContextIO (insertProject project10) requestContext
  runInContextIO Document.runMigration requestContext

documentArguments :: [(K.Key, Value)] -> Value
documentArguments arguments = object (("document_uuid" .= doc1.uuid) : arguments)

documentUuidsOf :: SResponse -> [Maybe Value]
documentUuidsOf response =
  case valueField "documents" =<< valueField "_embedded" =<< structuredContentOf response of
    Just (Array documents) -> fmap (valueField "uuid") (V.toList documents)
    _ -> []

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfDocumentInDB :: RequestContext -> Document -> WaiSession () ()
assertExistenceOfDocumentInDB requestContext expDocument = do
  document <- getOneFromDB (findDocumentByUuid expDocument.uuid) requestContext
  liftIO $ document `shouldBe` expDocument

assertAbsenceOfDocumentInDB :: RequestContext -> U.UUID -> WaiSession () ()
assertAbsenceOfDocumentInDB requestContext documentUuid = do
  mDocument <- getOneFromDB (findDocumentByUuid' documentUuid) requestContext
  liftIO $ fmap (.uuid) mDocument `shouldBe` Nothing
