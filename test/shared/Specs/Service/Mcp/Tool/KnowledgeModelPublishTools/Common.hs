module Specs.Service.Mcp.Tool.KnowledgeModelPublishTools.Common where

import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertAbsenceOfPublishedPackageInDB :: RequestContext -> WaiSession () ()
assertAbsenceOfPublishedPackageInDB requestContext = do
  editor <- getOneFromDB (findKnowledgeModelEditorByUuid amsterdamKnowledgeModelEditorList.uuid) requestContext
  ePackages <- runInContextIO (findPackagesById editor.id Nothing) requestContext
  liftIO $ fmap length ePackages `shouldBe` Right 0

assertPublishedPackageInDB :: RequestContext -> U.UUID -> WaiSession () ()
assertPublishedPackageInDB requestContext packageUuid = do
  packageFromDb <- getOneFromDB (findPackageByUuid packageUuid) requestContext
  editor <- getOneFromDB (findKnowledgeModelEditorByUuid amsterdamKnowledgeModelEditorList.uuid) requestContext
  liftIO $ (packageFromDb.id, packageFromDb.version) `shouldBe` (editor.id, editor.version)
  liftIO $ editor.previousPackageUuid `shouldBe` Just packageUuid

assertKnowledgeModelLocaleCodesInDB :: RequestContext -> U.UUID -> [String] -> WaiSession () ()
assertKnowledgeModelLocaleCodesInDB requestContext packageUuid expCodes = do
  locales <- getOneFromDB (findKnowledgeModelLocalesByPackageUuid packageUuid) requestContext
  liftIO $ fmap (.code) locales `shouldBe` expCodes

assertExistenceOfPackageUuidInDB :: RequestContext -> U.UUID -> WaiSession () ()
assertExistenceOfPackageUuidInDB requestContext packageUuid = do
  packageFromDb <- getOneFromDB (findPackageByUuid packageUuid) requestContext
  liftIO $ packageFromDb.uuid `shouldBe` packageUuid
