module Specs.Api.Handler.DocumentTemplate.Detail_DELETE (
  detail_DELETE,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.WizardPublic
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplate.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/document-templates/{uuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/document-templates/{uuid}" $ do
    test_204 requestContext
    test_204_all_versions requestContext
    test_204_dependents requestContext
    test_400_hidden_dependents requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = BS.pack $ "/api/document-templates/" ++ U.toString wizardDocumentTemplate.uuid

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext = create_test_204 "HTTP 204 NO CONTENT" requestContext reqAuthHeader

create_test_204 title requestContext reqAuthHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDocumentTemplates requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204_all_versions requestContext =
  it "HTTP 204 NO CONTENT (allVersions=true deletes every released version and keeps the draft)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      runInContextIO (insertDocumentTemplate wizardDocumentTemplateV1_1) requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "?allVersions=true") (reqHeadersT reqAuthHeader) reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      templatesFromDb <- getOneFromDB (findDocumentTemplatesByIdInWorkspace "global.project-report" Nothing) requestContext
      liftIO $ fmap (.uuid) templatesFromDb `shouldBe` [wizardDocumentTemplateDraft.uuid]

wizardDocumentTemplateV1_1 = wizardDocumentTemplate {uuid = u' "0b1c3c5e-6a7d-4f0e-9a1b-2c3d4e5f6a7b", version = "1.1.0"} :: DocumentTemplate

test_204_dependents requestContext =
  it "HTTP 204 NO CONTENT (documents are deleted, projects lose the template)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runDependentsMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl (reqHeadersT reqAuthHeader) reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDocumentTemplates requestContext 0
      assertCountInDB findDocuments requestContext 0
      projectFromDb <- getOneFromDB (findProjectByUuid project4.uuid) requestContext
      liftIO $ projectFromDb.documentTemplateUuid `shouldBe` Nothing
      liftIO $ projectFromDb.formatUuid `shouldBe` Nothing

test_400_hidden_dependents requestContext =
  it "HTTP 400 BAD REQUEST when a dependent is hidden from the caller" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError $ _ERROR_SERVICE_LIBRARY__DELETE_HIDDEN_DEPENDENTS 0 0 1 1 1
      let expBody = encode expDto
      -- AND: Run migrations
      runDependentsMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl (reqHeadersT reqNonAdminAuthHeader) reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDocumentTemplates requestContext 1
      assertCountInDB findDocuments requestContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "documentTemplates.manage"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/document-templates/3db4265e-8ba2-433d-97fb-6cc504866bbd"
    (reqHeadersT reqAuthHeader)
    reqBody
    "document_template"
    [("uuid", "3db4265e-8ba2-433d-97fb-6cc504866bbd")]
