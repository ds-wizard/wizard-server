module Specs.Api.Handler.KnowledgeModelPackage.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Util.String (replace)
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelPackage.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/knowledge-model-packages
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /wizard-api/knowledge-model-packages" $ do
    test_201_req_all_db_all requestContext
    test_201_req_all_db_no requestContext
    test_201_req_no_db_all requestContext
    test_201_req_one_db_rest requestContext
    test_201_without_readme requestContext
    test_400 requestContext
    test_400_main_package_duplication requestContext
    test_400_missing_previous_package requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/knowledge-model-packages"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = netherlandsV2KmBundle

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_req_all_db_all requestContext =
  it "HTTP 201 CREATED - In request: all previous packages, in DB: all previous packages" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = toSimpleDTO netherlandsKmPackageV2
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages requestContext
      runInContextIO (insertPackage globalKmPackage) requestContext
      runInContextIO (insertPackage netherlandsKmPackage) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelPackageSimpleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      comparePackageDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages requestContext 3
      assertExistenceOfBundlePackageInDB requestContext (head reqDto.packages)
      assertExistenceOfBundlePackageInDB requestContext (reqDto.packages !! 1)
      assertExistenceOfBundlePackageInDB requestContext (reqDto.packages !! 2)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_req_all_db_no requestContext =
  it "HTTP 201 CREATED - In request: all previous packages, in DB: no previous packages" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = toSimpleDTO netherlandsKmPackageV2
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelPackageSimpleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      comparePackageDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages requestContext 3
      assertExistenceOfBundlePackageInDB requestContext globalKmPackage
      assertExistenceOfBundlePackageInDB requestContext netherlandsKmPackage
      assertExistenceOfBundlePackageInDB requestContext netherlandsKmPackageV2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_req_no_db_all requestContext =
  it "HTTP 201 CREATED - In request: no previous packages, in DB: all previous packages" $
    -- GIVEN: Prepare request
    do
      let reqDto = netherlandsV2KmBundle {packages = [netherlandsV2KmBundlePackage]}
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = toSimpleDTO netherlandsKmPackageV2
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages requestContext
      runInContextIO (insertPackage globalKmPackage) requestContext
      runInContextIO (insertPackage netherlandsKmPackage) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelPackageSimpleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      comparePackageDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages requestContext 3
      assertExistenceOfBundlePackageInDB requestContext globalKmPackage
      assertExistenceOfBundlePackageInDB requestContext netherlandsKmPackage
      assertExistenceOfBundlePackageInDB requestContext netherlandsKmPackageV2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_req_one_db_rest requestContext =
  it "HTTP 201 CREATED - In request: one previous package, in DB: rest of previous packages" $
    -- GIVEN: Prepare request
    do
      let reqDto = netherlandsV2KmBundle {packages = [netherlandsKmBundlePackage, netherlandsV2KmBundlePackage]}
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = toSimpleDTO netherlandsKmPackageV2
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages requestContext
      runInContextIO (insertPackage globalKmPackage) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelPackageSimpleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      comparePackageDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages requestContext 3
      assertExistenceOfBundlePackageInDB requestContext globalKmPackage
      assertExistenceOfBundlePackageInDB requestContext netherlandsKmPackage
      assertExistenceOfBundlePackageInDB requestContext netherlandsKmPackageV2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_without_readme requestContext =
  it "HTTP 201 CREATED - Without 'readme' field" $
    -- GIVEN: Prepare request
    do
      let reqDto = netherlandsV2KmBundle {packages = [netherlandsKmBundlePackage, netherlandsV2KmBundlePackage]}
      let reqBody = BSL.pack . replace "readme" "differentReadme" . BSL.unpack $ encode reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = toSimpleDTO $ netherlandsKmPackageV2 {readme = ""}
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages requestContext
      runInContextIO (insertPackage globalKmPackage) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelPackageSimpleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      comparePackageDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages requestContext 3
      assertExistenceOfBundlePackageInDB requestContext expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST when json is not valid" $ do
    let reqHeaders = [reqAuthHeader, reqCtHeader]
    let reqBody = BSL.pack "{}"
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError . _ERROR_UTIL_JSON__MISSING_FIELD_IN_OBJECT $ "packages"
    let expBody = encode expDto
    -- WHEN: Call APIA
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_main_package_duplication requestContext =
  it "HTTP 400 BAD REQUEST when main package already exists" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS (show . createCoordinate $ netherlandsKmPackageV2)
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages requestContext
      runInContextIO (insertPackage globalKmPackage) requestContext
      runInContextIO (insertPackage netherlandsKmPackage) requestContext
      runInContextIO (insertPackage netherlandsKmPackageV2) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages requestContext 3

test_400_missing_previous_package requestContext =
  it "HTTP 400 BAD REQUEST when missing previous package" $
    -- GIVEN: Prepare request
    do
      let reqDto = netherlandsV2KmBundle {packages = [netherlandsV2KmBundlePackage]}
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            UserError $
              _ERROR_SERVICE_PKG__IMPORT_PREVIOUS_PKG_AT_FIRST (show . createCoordinate $ netherlandsKmPackage) (show . createCoordinate $ netherlandsKmPackageV2)
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO deletePackages requestContext
      runInContextIO (insertPackage globalKmPackage) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findPackages requestContext 1
      assertExistenceOfBundlePackageInDB requestContext globalKmPackage

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
