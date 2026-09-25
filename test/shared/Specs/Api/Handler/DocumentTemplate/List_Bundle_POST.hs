module Specs.Api.Handler.DocumentTemplate.List_Bundle_POST (
  list_bundle_POST,
) where

import Control.Monad (when)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Model.Error.Error
import Shared.Model.User.RolePermission
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/document-templates/bundle
-- ------------------------------------------------------------------------
list_bundle_POST :: RequestContext -> SpecWith ((), Application)
list_bundle_POST requestContext =
  describe "POST /api/document-templates/bundle" $ do
    test_201 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/document-templates/bundle"

boundary = "X-TEST-BOUNDARY"

reqCtMultipartHeader = ("Content-Type", BS.pack $ "multipart/form-data; boundary=" ++ boundary)

reqHeaders = [reqAuthHeader, reqCtMultipartHeader]

reqBody = createMultipartBody (toDocumentTemplateArchive (toBundle anotherWizardDocumentTemplate [] [] []) [])

createMultipartBody :: BSL.ByteString -> BSL.ByteString
createMultipartBody content =
  BSL.concat
    [ BSL.pack $ "--" ++ boundary ++ "\r\n"
    , BSL.pack "Content-Disposition: form-data; name=\"file\"; filename=\"template.zip\"\r\n"
    , BSL.pack "Content-Type: application/zip\r\n\r\n"
    , content
    , BSL.pack "\r\n"
    , BSL.pack $ "--" ++ boundary ++ "--\r\n"
    ]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext = do
  create_test_201 "HTTP 201 CREATED (tenant plane)" requestContext False (reqUrl <> "?tenant=true") Nothing
  create_test_201 "HTTP 201 CREATED (workspace plane)" requestContext True (reqUrl <> "?w=7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f") (Just defaultWorkspaceUuid)

create_test_201 title requestContext multiWorkspace reqUrl expWorkspaceUuid =
  it title $
    -- GIVEN: Prepare expectation
    do
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
      liftIO $ resDto.name `shouldBe` anotherWizardDocumentTemplate.name
      -- AND: Find result in DB and compare with expectation state
      templateFromDb <- getOneFromDB (findDocumentTemplateByUuid resDto.uuid) requestContext
      liftIO $ templateFromDb.templateId `shouldBe` anotherWizardDocumentTemplate.templateId
      liftIO $ templateFromDb.workspaceUuid `shouldBe` expWorkspaceUuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST - no plane in a multi-workspace tenant" $
    -- GIVEN: Prepare expectation
    do
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
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtMultipartHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtMultipartHeader] reqBody _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION
