module Specs.Api.Handler.DocumentTemplateDraft.Asset.Detail_DELETE (
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

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Tenant
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Localization.Messages.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplateDraft.Asset.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/document-template-drafts/{dtUuid}/assets/{assetUuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/document-template-drafts/{dtUuid}/assets/{assetUuid}" $ do
    test_204 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = BS.pack $ "/api/document-template-drafts/" ++ U.toString wizardDocumentTemplate.uuid ++ "/assets/" ++ U.toString assetLogo.uuid

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
      runInContextIO TML_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertAbsenceOfTemplateAssetInDB requestContext assetLogo

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "documentTemplates.useEditor"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext = do
  createNotFoundTest'
    reqMethod
    (BS.pack $ "/api/document-template-drafts/" ++ U.toString wizardDocumentTemplate.uuid ++ "/assets/b918fa4a-b0cf-4bef-ada6-e8cb3848cc75")
    (reqHeadersT reqAuthHeader)
    reqBody
    "document_template"
    [("uuid", U.toString wizardDocumentTemplate.uuid)]
  it "HTTP 404 NOT FOUND (asset of another document template)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = BS.pack $ "/api/document-template-drafts/" ++ U.toString wizardDocumentTemplateDraft.uuid ++ "/assets/" ++ U.toString assetLogo.uuid
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            NotExistsError
              ( _ERROR_DATABASE__ENTITY_NOT_FOUND
                  "document_template_asset"
                  [ ("tenant_uuid", U.toString defaultTenantUuid)
                  , ("document_template_uuid", U.toString wizardDocumentTemplateDraft.uuid)
                  , ("uuid", U.toString assetLogo.uuid)
                  ]
              )
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfTemplateAssetInDB requestContext assetLogo
