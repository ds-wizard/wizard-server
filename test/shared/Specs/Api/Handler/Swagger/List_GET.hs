module Specs.Api.Handler.Swagger.List_GET (
  list_GET,
) where

import qualified Data.HashSet.InsOrd as InsOrdHS
import Data.List (sortOn)
import Data.Swagger
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common

-- ------------------------------------------------------------------------
-- GET /api/swaggers
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/swagger.json" $ do
    test_200 requestContext
    test_200_sorted_tags requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/swagger.json"

reqHeaders = [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, String)
      assertResStatus status expStatus

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_sorted_tags requestContext =
  it "HTTP 200 OK - tags are listed alphabetically" $
    -- WHEN: Call API
    do
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      (_, _, swagger) <- destructResponse' response :: WaiSession () (Int, ResponseHeaders, Swagger)
      let names = fmap _tagName . InsOrdHS.toList $ swagger._swaggerTags
      liftIO $ names `shouldNotBe` []
      liftIO $ names `shouldBe` sortOn T.toLower names
      liftIO $ names `shouldContain` ["Document Template"]
