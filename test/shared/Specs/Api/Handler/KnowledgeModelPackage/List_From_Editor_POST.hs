module Specs.Api.Handler.KnowledgeModelPackage.List_From_Editor_POST (
  list_from_editor_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Shared.S3.KnowledgeModel.KnowledgeModelLocaleS3
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Service.KnowledgeModel.Publish.KnowledgeModelPublishService
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelPackage.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/package/from-editor
-- ------------------------------------------------------------------------
list_from_editor_POST :: RequestContext -> SpecWith ((), Application)
list_from_editor_POST requestContext =
  describe "POST /wizard-api/package/from-editor" $ do
    test_201 requestContext
    test_201_with_locales requestContext
    test_400_not_reusable_locales requestContext
    test_400_invalid_json requestContext
    test_400_not_higher_pkg_version requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/knowledge-model-packages/from-editor"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = packagePublishEditorDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = toSimpleDTO amsterdamKmPackage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelPackageSimpleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      comparePackageDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfBundlePackageInDB requestContext expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_with_locales requestContext =
  it "HTTP 201 CREATED (with reused locales)" $
    -- GIVEN: Prepare request
    do
      let reqDtoWithLocales = packagePublishEditorDTO {localeUuids = Just [czechNetherlandsKmLocale.uuid]} :: PackagePublishEditorDTO
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (insertKnowledgeModelLocale czechNetherlandsKmLocale) requestContext
      runInContextIO (putKnowledgeModelLocale czechNetherlandsKmLocale.uuid translationPoFileName czechPoContent) requestContext
      runInContextIO (putKnowledgeModelLocale czechNetherlandsKmLocale.uuid translationJsonFileName czechJsonContent) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (encode reqDtoWithLocales)
      -- THEN: Compare response with expectation
      let (status, _, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelPackageSimpleDTO)
      assertResStatus status 201
      -- AND: Locales are copied to the published package
      eLocales <- runInContextIO (findKnowledgeModelLocalesByPackageUuid resBody.uuid) requestContext
      liftIO $ fmap (fmap (.code)) eLocales `shouldBe` Right ["cs"]
      liftIO $ fmap (fmap ((/= czechNetherlandsKmLocale.uuid) . (.uuid))) eLocales `shouldBe` Right [True]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_not_reusable_locales requestContext =
  it "HTTP 400 BAD REQUEST when selected locales cannot be reused" $
    -- GIVEN: Prepare request
    do
      let reqDtoWithLocales = packagePublishEditorDTO {localeUuids = Just [czechGlobalKmLocale.uuid]} :: PackagePublishEditorDTO
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_VALIDATION__KM_LOCALE_NOT_REUSABLE
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (encode reqDtoWithLocales)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json requestContext = createInvalidJsonTest reqMethod reqUrl "description"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_not_higher_pkg_version requestContext =
  it "HTTP 400 BAD REQUEST when version is not higher than the previous one" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (publishPackageFromKnowledgeModelEditor packagePublishEditorDTO) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "KnowledgeModelEditorsUseRolePermission"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/knowledge-model-packages/from-editor"
    reqHeaders
    reqBody
    "knowledge_model_editor"
    [("uuid", "6474b24b-262b-42b1-9451-008e8363f2b6")]
