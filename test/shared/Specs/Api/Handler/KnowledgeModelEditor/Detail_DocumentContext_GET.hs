module Specs.Api.Handler.KnowledgeModelEditor.Detail_DocumentContext_GET (
  detail_documentContext_GET,
) where

import Data.Aeson (Value (..), toJSON)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Model.Document.DocumentContextJM ()
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import Shared.Service.Document.Context.DocumentContextHash
import Shared.Service.Document.Context.DocumentContextService
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/knowledge-model-editors/{uuid}/document-context
-- ------------------------------------------------------------------------
detail_documentContext_GET :: RequestContext -> SpecWith ((), Application)
detail_documentContext_GET requestContext =
  describe "GET /api/knowledge-model-editors/{uuid}/document-context" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/knowledge-model-editors/6474b24b-262b-42b1-9451-008e8363f2b6/document-context"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      (Right expCtx) <- runInContextIO (createKnowledgeModelEditorDocumentContext amsterdamKnowledgeModelEditor) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      result <- destructResponse' response
      let (status, headers, resBody) = result :: (Int, ResponseHeaders, Value)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      assertEtagHeaders headers (jsonString . jsonField "hash" $ resBody)
      liftIO $ resBody `shouldBe` toJSON expCtx
      liftIO $ (jsonString . jsonField "hash" $ resBody) `shouldBe` computeContextValueHash resBody
      liftIO $ jsonField "project" resBody `shouldBe` Null
      liftIO $ jsonField "document" resBody `shouldBe` Null
      let editor = jsonField "knowledgeModelEditor" resBody
      liftIO $ jsonField "id" editor `shouldBe` toJSON amsterdamKnowledgeModelEditor.id
      liftIO $ jsonField "version" editor `shouldBe` toJSON amsterdamKnowledgeModelEditor.version
      liftIO $ jsonField "license" editor `shouldBe` toJSON amsterdamKnowledgeModelEditor.license
      liftIO $ jsonField "readme" editor `shouldBe` toJSON amsterdamKnowledgeModelEditor.readme
      liftIO $ jsonField "previousPackageUuid" editor `shouldBe` toJSON netherlandsKmPackage.uuid
      liftIO $ jsonField "previousPackageId" editor `shouldBe` toJSON netherlandsKmPackage.id
      liftIO $ jsonField "previousPackageVersion" editor `shouldBe` toJSON netherlandsKmPackage.version
      liftIO $ jsonField "knowledgeModelMetamodelVersion" editor `shouldBe` toJSON amsterdamKnowledgeModelEditor.metamodelVersion
      liftIO $ jsonField "uuid" (jsonField "knowledgeModelPackage" resBody) `shouldBe` toJSON netherlandsKmPackage.uuid
      let questionnaire = jsonField "questionnaire" resBody
      liftIO $ jsonField "labels" questionnaire `shouldBe` Object mempty
      liftIO $ jsonField "versions" questionnaire `shouldBe` Array mempty
      liftIO $ jsonField "files" questionnaire `shouldBe` Array mempty
      liftIO $ jsonField "phaseUuid" questionnaire `shouldBe` Null

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/knowledge-model-editors/dc9fe65f-748b-47ec-b30c-d255bbac64a0/document-context"
    reqHeaders
    reqBody
    "knowledge_model_editor"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
