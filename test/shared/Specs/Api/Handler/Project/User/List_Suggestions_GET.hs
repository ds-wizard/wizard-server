module Specs.Api.Handler.Project.User.List_Suggestions_GET (
  list_suggestions_GET,
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
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.User.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Service.User.WizardUserMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/projects/{projectUuid}/users/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: RequestContext -> SpecWith ((), Application)
list_suggestions_GET requestContext =
  describe "GET /wizard-api/projects/{projectUuid}/users/suggestions" $ do
    test_200 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT projectUuid query = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/users/suggestions?sort=uuid,asc" ++ query

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
    [reqAuthHeader]
    (Page "users" (PageMetadata 20 1 1 0) (fmap (toSuggestion . toSimple) [userAlbert]))
  create_test_200
    "HTTP 200 OK (Commenter)"
    requestContext
    (project13 {visibility = PrivateProjectVisibility})
    [reqNonAdminAuthHeader]
    (Page "users" (PageMetadata 20 1 1 0) (fmap (toSuggestion . toSimple) [userNikola]))
  create_test_200
    "HTTP 200 OK (Non-Commenter, VisibleComment)"
    requestContext
    project13
    [reqIsaacAuthTokenHeader]
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestion . toSimple) [userNikola, userIsaac, userAlbert]))
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleComment, AnyoneWithLinkComment)"
    requestContext
    (project13 {sharing = AnyoneWithLinkCommentProjectSharing})
    []
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestion . toSimple) [userNikola, userIsaac, userAlbert]))
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleEdit)"
    requestContext
    project3
    [reqNonAdminAuthHeader]
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestion . toSimple) [userNikola, userIsaac, userAlbert]))
  create_test_200
    "HTTP 200 OK (Anonymous, Public, Sharing)"
    requestContext
    project10
    []
    (Page "users" (PageMetadata 20 3 1 0) (fmap (toSuggestion . toSimple) [userNikola, userIsaac, userAlbert]))

create_test_200 title requestContext project authHeader expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid ""
      let reqHeaders = reqHeadersT authHeader
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO (insertProject project) requestContext
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
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
test_403 requestContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    requestContext
    project1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Comment Project")
  create_test_403
    "HTTP 200 OK (Non-Owner, VisibleView)"
    requestContext
    project2
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Comment Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    requestContext
    project2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 200 OK (Anonymous, VisibleView, Sharing)"
    requestContext
    project7
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
      let reqUrl = reqUrlT project.uuid ""
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
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
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/users/suggestions"
    [reqHeadersT reqAuthHeader]
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
