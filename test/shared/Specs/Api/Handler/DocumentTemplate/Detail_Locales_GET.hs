module Specs.Api.Handler.DocumentTemplate.Detail_Locales_GET (
  detail_locales_GET,
) where

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListJM ()
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/document-templates/{uuid}/locales
-- ------------------------------------------------------------------------
detail_locales_GET :: RequestContext -> SpecWith ((), Application)
detail_locales_GET requestContext =
  describe "GET /wizard-api/document-templates/{uuid}/locales" $ do
    test_200 requestContext
    test_401 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/wizard-api/document-templates/" ++ show wizardDocumentTemplate.uuid ++ "/locales"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = [czechWizardDocumentTemplateLocaleList]
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, [DocumentTemplateLocaleList])
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resBody `shouldBe` expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/document-templates/78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e/locales"
    reqHeaders
    reqBody
    "document_template"
    [("uuid", "78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e")]
