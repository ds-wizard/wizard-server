module Specs.Api.Handler.DocumentTemplate.Detail_Locales_DELETE (
  detail_locales_DELETE,
) where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (isNothing)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateLocaleDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /wizard-api/document-templates/{uuid}/locales/{localeUuid}
-- ------------------------------------------------------------------------
detail_locales_DELETE :: RequestContext -> SpecWith ((), Application)
detail_locales_DELETE requestContext =
  describe "DELETE /wizard-api/document-templates/{uuid}/locales/{localeUuid}" $ do
    test_204 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = BS.pack $ "/wizard-api/document-templates/" ++ show wizardDocumentTemplate.uuid ++ "/locales/" ++ show czechWizardDocumentTemplateLocale.uuid

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
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
      -- AND: Compare state in DB with expectation
      eLocaleFromDb <- runInContextIO (findDocumentTemplateLocaleByDocumentTemplateUuidAndCode' wizardDocumentTemplate.uuid "cs") requestContext
      liftIO $ fmap isNothing eLocaleFromDb `shouldBe` Right True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    (BS.pack $ "/wizard-api/document-templates/" ++ show wizardDocumentTemplate.uuid ++ "/locales/78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e")
    reqHeaders
    reqBody
    "document_template_locale"
    [("document_template_uuid", show wizardDocumentTemplate.uuid), ("uuid", "78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e")]
