module Specs.Api.Handler.DocumentTemplateDraft.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftDetail
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplate.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/document-template-drafts/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /wizard-api/document-template-drafts/{uuid}" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = BS.pack $ "/wizard-api/document-template-drafts/" ++ U.toString wizardDocumentTemplateDraft.uuid

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto = wizardDocumentTemplateDraftChangeDTO

reqBodyT = encode

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (change name)"
    requestContext
    (wizardDocumentTemplateDraftChangeDTO {name = "Some edited name"} :: DocumentTemplateDraftChangeDTO)
    (wizardDocumentTemplateDraft {name = "Some edited name"} :: DocumentTemplate)
  create_test_200
    "HTTP 200 OK (publish)"
    requestContext
    (wizardDocumentTemplateDraftChangeDTO {phase = ReleasedDocumentTemplatePhase} :: DocumentTemplateDraftChangeDTO)
    (wizardDocumentTemplateDraft {phase = ReleasedDocumentTemplatePhase} :: DocumentTemplate)
  create_test_200
    "HTTP 200 OK (publish)"
    requestContext
    (wizardDocumentTemplateDraftChangeDTO {version = "3.0.0"} :: DocumentTemplateDraftChangeDTO)
    (wizardDocumentTemplateDraft {version = "3.0.0"} :: DocumentTemplate)

create_test_200 title requestContext reqDto expDto =
  it title $
    do
      -- GIVEN: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO (insertProject project1) requestContext
      runInContextIO (insertDraftData wizardDocumentTemplateDraftData) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (reqBodyT reqDto)
      -- THEN: Compare response with expectation
      result <- destructResponse' response
      let (status, headers, resDto) = result :: (Int, ResponseHeaders, DocumentTemplateDraftDetail)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareTemplateDtos resDto expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfDocumentTemplateInDB requestContext wizardDocumentTemplateDeprecated

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] (reqBodyT reqDto)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] (reqBodyT reqDto) "DocumentTemplateEditorsUseRolePermission"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/document-template-drafts/3db4265e-8ba2-433d-97fb-6cc504866bbd"
    reqHeaders
    (reqBodyT reqDto)
    "document_template"
    [("uuid", "3db4265e-8ba2-433d-97fb-6cc504866bbd"), ("phase", "DraftDocumentTemplatePhase")]
