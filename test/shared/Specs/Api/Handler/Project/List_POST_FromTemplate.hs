module Specs.Api.Handler.Project.List_POST_FromTemplate (
  list_POST_fromTemplate,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.ProjectCreateFromTemplateDTO
import Shared.Api.Resource.Project.ProjectCreateJM ()
import Shared.Api.Resource.Project.ProjectDTO
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.Tenant.Config.WizardTenantConfig hiding (request)
import Shared.Service.Tenant.Config.ConfigService
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/projects?fromTemplate=true
-- ------------------------------------------------------------------------
list_POST_fromTemplate :: RequestContext -> SpecWith ((), Application)
list_POST_fromTemplate requestContext =
  describe "POST /wizard-api/projects/from-template" $ do
    test_201 requestContext
    test_400 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/projects/from-template"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT projectTemplateUuid name =
  ProjectCreateFromTemplateDTO
    { name = name
    , projectUuid = projectTemplateUuid
    }

reqBodyT projectTemplateUuid name = encode (reqDtoT projectTemplateUuid name)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT project1.uuid project11.name
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = project11Dto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareProjectCreateFromTemplateDtos resBody expDto
      -- AND: Find a result in DB
      assertCountInDB findProjects requestContext 4

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST (projectCreation: CustomProjectCreation)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT project2.uuid project11.name
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Project Template"
      let expBody = encode expDto
      -- AND: Change tenantConfig
      (Right tcProject) <- runInContextIO getCurrentTenantConfigProject requestContext
      let tcProjectUpdated = tcProject {projectCreation = CustomProjectCreation}
      runInContextIO (modifyTenantConfigProject tcProjectUpdated) requestContext
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertCountInDB findProjects requestContext 3

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext =
  it "HTTP 403 FORBIDDEN (isTemplate: False)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT project2.uuid project11.name
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Project Template"
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertCountInDB findProjects requestContext 3
