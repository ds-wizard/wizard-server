module Specs.Api.Handler.DocumentTemplateDraft.Asset.Detail_Content_GET (
  detail_content_GET,
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
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/document-template-drafts/{dtUuid}/assets/{assetUuid}/content
-- ------------------------------------------------------------------------
detail_content_GET :: RequestContext -> SpecWith ((), Application)
detail_content_GET requestContext =
  describe "GET /api/document-template-drafts/{dtUuid}/assets/{assetUuid}/content" $ do
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT dtUuid assetUuid = BS.pack $ "/api/document-template-drafts/" ++ U.toString dtUuid ++ "/assets/" ++ U.toString assetUuid ++ "/content"

reqUrl = reqUrlT wizardDocumentTemplate.uuid assetLogo.uuid

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody "documentTemplates.useEditor"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  it "HTTP 404 NOT FOUND (asset of another document template)" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT wizardDocumentTemplateDraft.uuid assetLogo.uuid
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
