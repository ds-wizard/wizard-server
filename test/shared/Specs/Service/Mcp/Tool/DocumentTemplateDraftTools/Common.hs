module Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.Common where

import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runDraftMigrations requestContext = do
  runInContextIO DocumentTemplate.runMigration requestContext
  runInContextIO (insertPackage germanyKmPackage) requestContext
  runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
  runInContextIO (insertProject project1) requestContext
  runInContextIO (insertDraftData wizardDocumentTemplateDraftData) requestContext

newDraftUuid :: U.UUID
newDraftUuid = u' "8f2a4c6e-1b3d-4f5a-9c7e-2d4f6a8b0c1e"

newDraft :: DocumentTemplate
newDraft =
  wizardDocumentTemplateDraft
    { uuid = newDraftUuid
    , name = "New template"
    , id = "org.new-template"
    , version = "1.0.0"
    , description = ""
    , readme = ""
    , license = ""
    , allowedPackages = []
    , workspaceUuid = Nothing
    }

draftFile :: DocumentTemplateFile
draftFile = fileDefaultHtml {documentTemplateUuid = wizardDocumentTemplateDraft.uuid, uuid = u' "5e7c1a3f-9b2d-4e6a-8c0f-1d3b5a7c9e2f"}

draftAsset :: DocumentTemplateAsset
draftAsset = assetLogo {documentTemplateUuid = wizardDocumentTemplateDraft.uuid, uuid = u' "3a5c7e9b-2d4f-4b6a-8e1c-5f7a9b1d3e6c"}

arrayItems :: Value -> [Value]
arrayItems (Array items) = V.toList items
arrayItems _ = []

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfDocumentTemplateDraftInDB :: RequestContext -> DocumentTemplate -> WaiSession () ()
assertExistenceOfDocumentTemplateDraftInDB requestContext expDraft = do
  draft <- getOneFromDB (findDraftByUuid expDraft.uuid) requestContext
  liftIO $ draft `shouldBe` expDraft {tenantUuid = draft.tenantUuid, createdAt = draft.createdAt, updatedAt = draft.updatedAt}

assertExistenceOfDocumentTemplateInDB :: RequestContext -> DocumentTemplate -> WaiSession () ()
assertExistenceOfDocumentTemplateInDB requestContext expDocumentTemplate = do
  documentTemplate <- getOneFromDB (findDocumentTemplateByUuid expDocumentTemplate.uuid) requestContext
  liftIO $ documentTemplate `shouldBe` expDocumentTemplate {createdAt = documentTemplate.createdAt, updatedAt = documentTemplate.updatedAt}

assertDocumentTemplateFormatsInDB :: RequestContext -> U.UUID -> [(U.UUID, String, [String])] -> WaiSession () ()
assertDocumentTemplateFormatsInDB requestContext documentTemplateUuid expFormats = do
  formats <- getOneFromDB (findDocumentTemplateFormats documentTemplateUuid) requestContext
  liftIO $ fmap (\format -> (format.uuid, format.name, fmap (.name) format.steps)) formats `shouldMatchList` expFormats

assertDocumentTemplateFormatNamesInDB :: RequestContext -> U.UUID -> [String] -> WaiSession () ()
assertDocumentTemplateFormatNamesInDB requestContext documentTemplateUuid expNames = do
  formats <- getOneFromDB (findDocumentTemplateFormats documentTemplateUuid) requestContext
  liftIO $ fmap (.name) formats `shouldMatchList` expNames
