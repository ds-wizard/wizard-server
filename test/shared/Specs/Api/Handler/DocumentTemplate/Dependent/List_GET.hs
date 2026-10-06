module Specs.Api.Handler.DocumentTemplate.Dependent.List_GET (
  list_GET,
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
import Shared.Api.Resource.Library.LibraryDependentsJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackageDependents
import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Library.LibraryDependents
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplate.Common

-- ------------------------------------------------------------------------
-- GET /api/document-templates/{uuid}/dependents
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/document-templates/{uuid}/dependents" $ do
    test_200 requestContext
    test_200_hidden requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/api/document-templates/" ++ U.toString wizardDocumentTemplate.uuid ++ "/dependents"

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            LibraryDependents
              { knowledgeModelPackages = []
              , editors = []
              , projects = [project4Dependent]
              , documents = [project4DocumentDependent]
              , hidden = noHiddenDependents
              , deleteAllowed = True
              }
      let expBody = encode expDto
      -- AND: Run migrations
      runDependentsMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl [reqAuthHeader] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_200_hidden requestContext = do
  it "HTTP 200 OK (private project of someone else and its documents are only counted)" $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            LibraryDependents
              { knowledgeModelPackages = []
              , editors = []
              , projects = []
              , documents = []
              , hidden = noHiddenDependents {projects = 1, documents = 1, workspaces = 1}
              , deleteAllowed = False
              }
      let expBody = encode expDto
      -- AND: Run migrations
      runDependentsMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl [reqNonAdminAuthHeader] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

project4DocumentDependent = LibraryDependentResource {uuid = project4Document.uuid, name = project4Document.name}

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody "documentTemplates.manage"
