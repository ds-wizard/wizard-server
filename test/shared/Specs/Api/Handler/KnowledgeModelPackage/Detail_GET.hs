module Specs.Api.Handler.KnowledgeModelPackage.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.WizardKnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as R
import Shared.Localization.Messages.User.Public
import Shared.Model.Error.Error
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-packages/{uuid}
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /wizard-api/knowledge-model-packages/{uuid}" $ do
    test_200 requestContext
    test_200_with_locales requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT pkgUuid = BS.pack $ "/wizard-api/knowledge-model-packages/" ++ show pkgUuid

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (public - with token)" requestContext [reqAuthHeader] globalKmPackageDetailDto
  create_test_200 "HTTP 200 OK (public - without token)" requestContext [] globalKmPackageDetailDto
  create_test_200 "HTTP 200 OK (private - with token)" requestContext [reqAuthHeader] globalNetherlandsPackageDetailDto

create_test_200 title requestContext authHeader expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      let reqUrl = reqUrlT expDto.uuid
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration requestContext
      runInContextIO R.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_with_locales requestContext =
  it "HTTP 200 OK (with locales)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = globalKmPackageDetailDto {locales = [czechGlobalKmLocaleList]} :: KnowledgeModelPackageDetailDTO
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration requestContext
      runInContextIO R.runMigration requestContext
      runInContextIO (insertKnowledgeModelLocale czechGlobalKmLocale) requestContext
      -- WHEN: Call API
      response <- request reqMethod (reqUrlT expDto.uuid) (reqHeadersT [reqAuthHeader]) reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  it "HTTP 403 FORBIDDEN - private" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT []
      let reqUrl = reqUrlT globalNetherlandsPackageDetailDto.uuid
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError _ERROR_SERVICE_USER__MISSING_USER
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration requestContext
      runInContextIO R.runMigration requestContext
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
    "/wizard-api/knowledge-model-packages/23cb2e5b-23e6-4402-b591-61dc74eba9bb"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "knowledge_model_package"
    [("uuid", "23cb2e5b-23e6-4402-b591-61dc74eba9bb")]
