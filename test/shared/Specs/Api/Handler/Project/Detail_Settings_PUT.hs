module Specs.Api.Handler.Project.Detail_Settings_PUT (
  detail_settings_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.ProjectSettingsChangeDTO
import Shared.Constant.Workspace
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.DocumentTemplate.Public
import Shared.Localization.Messages.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/projects/{projectUuid}/settings
-- ------------------------------------------------------------------------
detail_settings_PUT :: RequestContext -> SpecWith ((), Application)
detail_settings_PUT requestContext =
  describe "PUT /api/projects/{projectUuid}/settings" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrlT projectUuid = BS.pack $ "/api/projects/" ++ U.toString projectUuid ++ "/settings"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT project =
  ProjectSettingsChangeDTO
    { name = project.name
    , description = project.description
    , projectTags = project.projectTags
    , documentTemplateUuid = project.documentTemplateUuid
    , formatUuid = project.formatUuid
    , isTemplate = project.isTemplate
    , language = project.language
    , documentTemplateLanguage = project.documentTemplateLanguage
    }

reqBodyT project = encode $ reqDtoT project

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    requestContext
    project1
    project1SettingsEdited
    project1Events
    project1Ctn
    []
    True
    [reqAuthHeader]
    False
  create_test_200
    "HTTP 200 OK (Owner, VisibleView)"
    requestContext
    project2
    project2SettingsEdited
    project2Events
    project2Ctn
    []
    False
    [reqAuthHeader]
    False
  create_test_200
    "HTTP 200 OK (Non-Owner, Private, Sharing, Anonymous Enabled)"
    requestContext
    project10
    project10EditedSettings
    project10Events
    project10Ctn
    [project10NikolaEditProjectPermDto]
    False
    [reqNonAdminAuthHeader]
    True

create_test_200 title requestContext project projectEdited projectEvents projectContent permissions showComments authHeader anonymousEnabled =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      let reqBody = reqBodyT projectEdited
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = reqDtoT projectEdited
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (insertProject project10) requestContext
      runInContextIO (insertProjectEvents project10Events) requestContext
      -- AND: Enabled anonymous sharing
      updateAnonymousProjectSharing requestContext anonymousEnabled
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfProjectInDB requestContext projectEdited projectEvents

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod (reqUrlT project3.uuid) "visibility"
  it "HTTP 400 BAD REQUEST - Language without a locale on the document template" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project1.uuid
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqDto = (reqDtoT project1) {documentTemplateLanguage = Just "de"} :: ProjectSettingsChangeDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError $ _ERROR_VALIDATION__DOC_TML_LOCALE_NOT_AVAILABLE "de"
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
test_401 requestContext =
  createAuthTest reqMethod (reqUrlT project3.uuid) [reqCtHeader] (reqBodyT project1)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    requestContext
    project1
    project1SettingsEdited
    "Administrate Project"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    requestContext
    project2
    project2SettingsEdited
    "Administrate Project"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private, Sharing, Anonymous Disabled)"
    requestContext
    project10
    project10EditedSettings
    "Administrate Project"

create_test_403 title requestContext project projectEdited reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT [reqNonAdminAuthHeader]
      let reqBody = reqBodyT projectEdited
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (insertProject project10) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext = do
  createNotFoundTest'
    reqMethod
    "/api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/settings"
    (reqHeadersT [reqAuthHeader])
    (reqBodyT project1)
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
  it "HTTP 404 NOT FOUND (document template of another workspace)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project1.uuid
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqDto = (reqDtoT project1) {documentTemplateUuid = Just secondWorkspaceDocumentTemplate.uuid} :: ProjectSettingsChangeDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = NotExistsError (_ERROR_VALIDATION__ABSENCE "document_template")
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      insertSecondWorkspace requestContext
      runInContextIO (insertDocumentTemplate secondWorkspaceDocumentTemplate) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfProjectInDB requestContext project1 project1Events

secondWorkspaceDocumentTemplate :: DocumentTemplate
secondWorkspaceDocumentTemplate =
  wizardDocumentTemplate
    { uuid = u' "6b5a4c3d-2e1f-4a0b-9c8d-7e6f5a4b3c2d"
    , workspaceUuid = Just secondWorkspaceUuid
    }
