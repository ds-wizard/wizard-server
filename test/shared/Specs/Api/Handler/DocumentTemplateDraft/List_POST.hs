module Specs.Api.Handler.DocumentTemplateDraft.List_POST (
  list_POST,
) where

import Control.Monad (when)
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateJM ()
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Model.Error.Error
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/document-template-drafts
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/document-template-drafts" $ do
    test_201 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/document-template-drafts"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqBodyT = encode

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext = do
  create_test_201 "HTTP 201 CREATED (without extending)" requestContext reqUrl False reqAuthHeader (wizardDocumentTemplateDraftCreateDTO {basedOn = Nothing} :: DocumentTemplateDraftCreateDTO) wizardDocumentTemplateNlDraft Nothing
  create_test_201 "HTTP 201 CREATED (tenant=true)" requestContext (reqUrl <> "?tenant=true") True reqAuthHeader (wizardDocumentTemplateDraftCreateDTO {basedOn = Nothing} :: DocumentTemplateDraftCreateDTO) wizardDocumentTemplateNlDraft Nothing
  create_test_201 "HTTP 201 CREATED (workspace plane)" requestContext (reqUrl <> "?w=7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f") True reqAuthHeader (wizardDocumentTemplateDraftCreateDTO {basedOn = Nothing} :: DocumentTemplateDraftCreateDTO) wizardDocumentTemplateNlDraft (Just defaultWorkspaceUuid)
  it "HTTP 404 NOT FOUND (tenant=true, based on a template of a workspace)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      let reqBody = reqBodyT (wizardDocumentTemplateDraftCreateDTO {basedOn = Just secondWorkspaceDocumentTemplate.uuid} :: DocumentTemplateDraftCreateDTO)
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (NotExistsError (_ERROR_VALIDATION__ABSENCE "document_template"))
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      insertSecondWorkspace requestContext
      runInContextIO (insertDocumentTemplate secondWorkspaceDocumentTemplate) requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "?tenant=true") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDrafts requestContext 1

-- create_test_201 "HTTP 201 CREATED (with extending)" requestContext reqAuthHeader wizardDocumentTemplateDraftCreateDTO

create_test_201 title requestContext reqUrl multiWorkspace reqAuthHeader reqDto expDto expWorkspaceUuid =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      let reqBody = reqBodyT reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      when multiWorkspace (enableMultiWorkspace requestContext)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, DocumentTemplateSimple)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.name `shouldBe` expDto.name
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDrafts requestContext 2
      draftFromDb <- getOneFromDB (findDraftByUuid resDto.uuid) requestContext
      liftIO $ draftFromDb.workspaceUuid `shouldBe` expWorkspaceUuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST - no plane in a multi-workspace tenant" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      let reqBody = reqBodyT (wizardDocumentTemplateDraftCreateDTO {basedOn = Nothing} :: DocumentTemplateDraftCreateDTO)
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (UserError _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED)
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] (reqBodyT wizardDocumentTemplateDraftCreateDTO)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] (reqBodyT wizardDocumentTemplateDraftCreateDTO) "documentTemplates.useEditor"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  it "HTTP 404 NOT FOUND (based on a template of another workspace)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      let reqBody = reqBodyT (wizardDocumentTemplateDraftCreateDTO {basedOn = Just secondWorkspaceDocumentTemplate.uuid} :: DocumentTemplateDraftCreateDTO)
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (NotExistsError (_ERROR_VALIDATION__ABSENCE "document_template"))
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      insertSecondWorkspace requestContext
      runInContextIO (insertDocumentTemplate secondWorkspaceDocumentTemplate) requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "?w=7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDrafts requestContext 1

secondWorkspaceDocumentTemplate :: DocumentTemplate
secondWorkspaceDocumentTemplate =
  wizardDocumentTemplate
    { uuid = u' "6b5a4c3d-2e1f-4a0b-9c8d-7e6f5a4b3c2d"
    , workspaceUuid = Just secondWorkspaceUuid
    }
