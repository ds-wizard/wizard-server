module Specs.Api.Handler.KnowledgeModelPackage.List_Bundle_POST (
  list_bundle_POST,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- POST /api/knowledge-model-packages/bundle
-- ------------------------------------------------------------------------
list_bundle_POST :: RequestContext -> SpecWith ((), Application)
list_bundle_POST requestContext =
  describe "POST /api/knowledge-model-packages/bundle" $ do
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/knowledge-model-packages/bundle"

boundary = "X-TEST-BOUNDARY"

reqCtMultipartHeader = ("Content-Type", BS.pack $ "multipart/form-data; boundary=" ++ boundary)

reqOwlBody =
  BSL.concat
    [ formField "rootElement" "http://example.com/ontology#Root"
    , formField "name" "OWL Knowledge Model"
    , formField "organizationId" "org.example"
    , formField "kmId" "owl-km"
    , formField "version" "1.0.0"
    , BSL.pack $ "--" ++ boundary ++ "\r\n"
    , BSL.pack "Content-Disposition: form-data; name=\"file\"; filename=\"ontology.owl\"\r\n"
    , BSL.pack "Content-Type: application/rdf+xml\r\n\r\n"
    , BSL.pack "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"></rdf:RDF>"
    , BSL.pack "\r\n"
    , BSL.pack $ "--" ++ boundary ++ "--\r\n"
    ]

formField :: String -> String -> BSL.ByteString
formField name value =
  BSL.pack $
    "--" ++ boundary ++ "\r\nContent-Disposition: form-data; name=\"" ++ name ++ "\"\r\n\r\n" ++ value ++ "\r\n"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtMultipartHeader] reqOwlBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtMultipartHeader] reqOwlBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
