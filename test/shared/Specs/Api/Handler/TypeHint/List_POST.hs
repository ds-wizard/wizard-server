module Specs.Api.Handler.TypeHint.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.TypeHint.Data.TypeHints
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.User.Public
import Shared.Model.Error.Error
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/type-hints
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /wizard-api/type-hints" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/type-hints"

reqHeadersT authHeader = reqCtHeader : authHeader

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (Project, Owner)" requestContext projectTypeHintRequest project15 [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Project, Editor)" requestContext projectTypeHintRequest project15 [reqNonAdminAuthHeader]
  create_test_200 "HTTP 200 OK (Project, Anonymous)" requestContext projectTypeHintRequest project15AnonymousEdit []
  create_test_200 "HTTP 200 OK (KM Editor-Integration)" requestContext kmEditorIntegrationTypeHintRequest project15 [reqAuthHeader]
  create_test_200 "HTTP 200 OK (KM Editor-Question)" requestContext kmEditorQuestionTypeHintRequest project15 [reqAuthHeader]

create_test_200 title requestContext reqDto project authHeader =
  it title $ do
    -- GIVEN: Prepare request
    let reqBody = encode reqDto
    let reqHeaders = reqHeadersT authHeader
    -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = [forestDatasetTypeHint, genomicDatasetTypeHint, animalsDatasetTypeHint]
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    runInContextIO TML.runMigration requestContext
    runInContextIO KnowledgeModelPackage.runMigration requestContext
    runInContextIO PRJ.runMigration requestContext
    runInContextIO KnowledgeModelEditor.runMigration requestContext
    runInContextIO (insertPackage germanyKmPackage) requestContext
    runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
    runInContextIO (insertProject project) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = do
  create_test_401 requestContext kmEditorIntegrationTypeHintRequest
  create_test_401 requestContext kmEditorQuestionTypeHintRequest

create_test_401 requestContext reqDto =
  createAuthTest reqMethod reqUrl [reqCtHeader] (encode reqDto)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403_project
    "HTTP 403 FORBIDDEN (Project, Non-Owner)"
    requestContext
    projectTypeHintRequest
    project15NoPerms
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403_project
    "HTTP 403 FORBIDDEN (Project, Viewer)"
    requestContext
    projectTypeHintRequest
    project15
    [reqIsaacAuthTokenHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403_project
    "HTTP 403 FORBIDDEN (Project, Anonymous)"
    requestContext
    projectTypeHintRequest
    project15
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403_project
    "HTTP 403 FORBIDDEN (Project, Anonymous Commenter)"
    requestContext
    projectTypeHintRequest
    project15AnonymousComment
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403_knowledge_model_editor requestContext kmEditorIntegrationTypeHintRequest
  create_test_403_knowledge_model_editor requestContext kmEditorQuestionTypeHintRequest

create_test_403_project title requestContext reqDto project authHeader reason =
  it title $ do
    -- GIVEN: Prepare request
    let reqBody = encode reqDto
    let reqHeaders = reqHeadersT authHeader
    -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = ForbiddenError reason
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    runInContextIO TML.runMigration requestContext
    runInContextIO KnowledgeModelPackage.runMigration requestContext
    runInContextIO PRJ.runMigration requestContext
    runInContextIO (insertProject project) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

create_test_403_knowledge_model_editor requestContext reqDto =
  createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] (encode reqDto) "KnowledgeModelEditorsUseRolePermission"
