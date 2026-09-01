module Specs.Api.Handler.Project.Detail_Questionnaire_GET (
  detail_questionnaire_GET,
) where

import Data.Aeson (Value, decodeStrict, encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectCommentDAO
import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.User.Public
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectContent
import Shared.S3.KnowledgeModel.KnowledgeModelLocaleS3
import qualified Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper as KMP
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/projects/{projectUuid}/questionnaire
-- ------------------------------------------------------------------------
detail_questionnaire_GET :: RequestContext -> SpecWith ((), Application)
detail_questionnaire_GET requestContext =
  describe "GET /wizard-api/projects/{projectUuid}/questionnaire" $ do
    test_200 requestContext
    test_200_with_locale requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/questionnaire"

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    requestContext
    project1
    project1Events
    project1Ctn
    germanyKmPackage
    True
    [reqAuthHeader]
    [project1AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleView)"
    requestContext
    project2
    project2Events
    (project2Ctn {labels = M.empty} :: ProjectContent)
    germanyKmPackage
    False
    [reqNonAdminAuthHeader]
    [project2AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Commenter)"
    requestContext
    (project13 {visibility = PrivateProjectVisibility})
    project13Events
    (project13Ctn {labels = M.empty} :: ProjectContent)
    germanyKmPackage
    True
    [reqNonAdminAuthHeader]
    [project13NikolaCommentProjectPermDto]
  create_test_200
    "HTTP 200 OK (Non-Commenter, VisibleComment)"
    requestContext
    project13
    project13Events
    (project13Ctn {labels = M.empty} :: ProjectContent)
    germanyKmPackage
    True
    [reqIsaacAuthTokenHeader]
    [project13NikolaCommentProjectPermDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleComment, AnyoneWithLinkComment)"
    requestContext
    (project13 {sharing = AnyoneWithLinkCommentProjectSharing})
    project13Events
    (project13Ctn {labels = M.empty} :: ProjectContent)
    germanyKmPackage
    True
    []
    [project13NikolaCommentProjectPermDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleView, Sharing)"
    requestContext
    project7
    project7Events
    (project7Ctn {labels = M.empty} :: ProjectContent)
    germanyKmPackage
    False
    []
    [project7AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleEdit)"
    requestContext
    project3
    project3Events
    project3Ctn
    germanyKmPackage
    True
    [reqNonAdminAuthHeader]
    []
  create_test_200
    "HTTP 200 OK (Anonymous, Public, Sharing)"
    requestContext
    project10
    project10Events
    project10Ctn
    germanyKmPackage
    True
    []
    []

create_test_200 title requestContext project projectEvents projectContent kmPackage showComments authHeader permissions =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      thread1 <- liftIO . create_cmtQ1_t1 $ project.uuid
      comment1 <- liftIO . create_cmtQ1_t1_1 $ thread1.uuid
      comment2 <- liftIO . create_cmtQ1_t1_2 $ thread1.uuid
      runInContextIO (insertProject project) requestContext
      runInContextIO (insertProjectEvents projectEvents) requestContext
      runInContextIO (insertProjectCommentThread thread1) requestContext
      runInContextIO (insertProjectComment comment1) requestContext
      runInContextIO (insertProjectComment comment2) requestContext
      let unresolvedCommentCounts =
            if showComments
              then M.fromList [(cmtQ1_path, M.fromList [(thread1.uuid, 2)])]
              else M.empty
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            ProjectDetailQuestionnaireDTO
              { uuid = project.uuid
              , name = project.name
              , visibility = project.visibility
              , sharing = project.sharing
              , knowledgeModelPackage = KMP.toSuggestion kmPackage
              , selectedQuestionTagUuids = project.selectedQuestionTagUuids
              , language = project.language
              , locale = Nothing
              , isTemplate = project.isTemplate
              , knowledgeModel = km1WithQ4
              , replies = fReplies
              , labels = projectContent.labels
              , phaseUuid = projectContent.phaseUuid
              , permissions = permissions
              , files = []
              , unresolvedCommentCounts = unresolvedCommentCounts
              , resolvedCommentCounts = M.empty
              , fileCount = 0
              }
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_with_locale requestContext = do
  it "HTTP 200 OK (with locale)" $
    -- GIVEN: Run migrations
    do
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO (insertProject (project1 {language = Just "cs"} :: Project)) requestContext
      runInContextIO (insertProjectEvents project1Events) requestContext
      let kmLocale = czechGlobalKmLocale {knowledgeModelPackageUuid = germanyKmPackage.uuid} :: KnowledgeModelLocale
      runInContextIO (insertKnowledgeModelLocale kmLocale) requestContext
      runInContextIO (putKnowledgeModelLocale kmLocale.uuid translationJsonFileName czechJsonContent) requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrlT project1.uuid) [reqAuthHeader] reqBody
      -- THEN: Compare response with expectation
      let (status, _, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDetailQuestionnaireDTO)
      liftIO $ status `shouldBe` 200
      liftIO $ resBody.language `shouldBe` Just "cs"
      liftIO $ resBody.locale `shouldBe` (decodeStrict czechJsonContent :: Maybe Value)
  it "HTTP 200 OK (with unknown language)" $
    -- GIVEN: Run migrations
    do
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO (insertProject (project1 {language = Just "xx"} :: Project)) requestContext
      runInContextIO (insertProjectEvents project1Events) requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrlT project1.uuid) [reqAuthHeader] reqBody
      -- THEN: Compare response with expectation
      let (status, _, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDetailQuestionnaireDTO)
      liftIO $ status `shouldBe` 200
      liftIO $ resBody.language `shouldBe` Just "xx"
      liftIO $ resBody.locale `shouldBe` Nothing

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
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    requestContext
    project2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    requestContext
    project3
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title requestContext project authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
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
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/questionnaire"
    [reqHeadersT reqAuthHeader]
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
