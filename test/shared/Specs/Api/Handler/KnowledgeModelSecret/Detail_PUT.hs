module Specs.Api.Handler.KnowledgeModelSecret.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeJM ()
import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretMigration as KMS_Migration
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelSecret.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/knowledge-model-secrets/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /wizard-api/knowledge-model-secrets/{uuid}" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/knowledge-model-secrets/171635b5-d5e7-4bba-8dd0-93765866aea1"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto = kmSecret1ChangeDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = kmSecret1Edited
    -- AND: Run migrations
    runInContextIO KMS_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelSecret)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareKnowledgeModelSecretDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfKnowledgeModelSecretInDB requestContext kmSecret1Edited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "KnowledgeModelsManageRolePermission"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/knowledge-model-secrets/345fbf3b-06d5-4660-a108-fd30deb1a44f"
    reqHeaders
    reqBody
    "knowledge_model_secret"
    [("uuid", "345fbf3b-06d5-4660-a108-fd30deb1a44f")]
