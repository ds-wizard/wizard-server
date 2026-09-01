module Specs.Api.Handler.ProjectCommentThread.List_GET (
  list_GET,
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
import Shared.Database.DAO.Project.ProjectCommentDAO
import Shared.Database.DAO.Project.ProjectCommentThreadDAO
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Project.Comment.ProjectComment
import Shared.Model.Project.Comment.ProjectCommentThreadAssigned
import Shared.Model.Project.Project
import Shared.Model.User.User
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/project-comment-threads
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /wizard-api/project-comment-threads" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/project-comment-threads?sort=updatedAt,desc"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO (insertProject project1) requestContext
      -- AND: Create thread without assignee
      thread1 <- liftIO . create_cmtQ1_t1 $ project1.uuid
      comment1_1 <- liftIO . create_cmtQ1_t1_1 $ thread1.uuid
      comment1_2 <- liftIO . create_cmtQ1_t1_2 $ thread1.uuid
      runInContextIO (insertProjectCommentThread thread1) requestContext
      runInContextIO (insertProjectComment comment1_1) requestContext
      runInContextIO (insertProjectComment comment1_2) requestContext
      -- AND: Create thread with assignee
      thread2 <- liftIO . create_cmtQ1_t1 $ project1.uuid
      comment2_1 <- liftIO . create_cmtQ1_t1_1 $ thread2.uuid
      comment2_2 <- liftIO . create_cmtQ1_t1_2 $ thread2.uuid
      runInContextIO (insertProjectCommentThread (thread2 {assignedTo = Just userAlbert.uuid})) requestContext
      runInContextIO (insertProjectComment comment2_1) requestContext
      runInContextIO (insertProjectComment comment2_2) requestContext
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = Page "projectCommentThreads" (PageMetadata 20 1 1 0) [cmtAssigned {commentThreadUuid = thread2.uuid}]
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
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody
