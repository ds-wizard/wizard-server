module Specs.Api.Handler.Locale.List_Bundle_POST (
  list_bundle_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Locale.LocaleSimpleJM ()
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Localization.Messages.Coordinate.Public
import Shared.Model.Error.Error
import Shared.Model.Locale.Locale
import Shared.Model.Locale.LocaleSimple
import Shared.Service.Locale.Bundle.LocaleBundleMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- POST /api/locales/bundle
-- ------------------------------------------------------------------------
list_bundle_POST :: RequestContext -> SpecWith ((), Application)
list_bundle_POST requestContext =
  describe "POST /api/locales/bundle" $ do
    test_201 requestContext
    test_400 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/locales/bundle"

boundary = "X-TEST-BOUNDARY"

reqCtMultipartHeader = ("Content-Type", BS.pack $ "multipart/form-data; boundary=" ++ boundary)

reqHeaders = [reqAuthHeader, reqCtMultipartHeader]

reqBody = createMultipartBody (toLocaleArchive localeNl localeNlContent localeNlContent)

createMultipartBody :: BSL.ByteString -> BSL.ByteString
createMultipartBody content =
  BSL.concat
    [ BSL.pack $ "--" ++ boundary ++ "\r\n"
    , BSL.pack "Content-Disposition: form-data; name=\"file\"; filename=\"locale.zip\"\r\n"
    , BSL.pack "Content-Type: application/zip\r\n\r\n"
    , content
    , BSL.pack "\r\n"
    , BSL.pack $ "--" ++ boundary ++ "--\r\n"
    ]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleSimple)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.code `shouldBe` localeNl.code
      -- AND: Find result in DB and compare with expectation state
      localeFromDb <- getOneFromDB (findLocaleByUuid resDto.uuid) requestContext
      liftIO $ localeFromDb.id `shouldBe` localeNl.id
      liftIO $ localeFromDb.version `shouldBe` localeNl.version

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST when id is not in valid format" $
    -- GIVEN: Prepare request
    do
      let reqBody = createMultipartBody (toLocaleArchive (localeNl {id = "a:b"} :: Locale) localeNlContent localeNlContent)
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (ValidationError [] (M.singleton "id" [_ERROR_VALIDATION__INVALID_COORDINATE_PART_FORMAT "id" "a:b"]))
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
