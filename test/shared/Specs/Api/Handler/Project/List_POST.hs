module Specs.Api.Handler.Project.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.ProjectCreateDTO
import Shared.Api.Resource.Project.ProjectCreateJM ()
import Shared.Api.Resource.Project.ProjectDTO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Project
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/projects
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /wizard-api/projects" $ do
    test_201 requestContext
    test_400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/projects"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT project = project

reqBodyT project = encode (reqDtoT project)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext = do
  create_test_201 requestContext "HTTP 201 CREATED (with token)" False project1Create [reqAuthHeader]
  create_test_201
    requestContext
    "HTTP 201 CREATED (without token)"
    True
    (project1Create {sharing = AnyoneWithLinkEditProjectSharing} :: ProjectCreateDTO)
    []

create_test_201 requestContext title anonymousSharingEnabled project authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      let reqBody = reqBodyT project
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto =
            if anonymousSharingEnabled
              then project1Dto {sharing = AnyoneWithLinkEditProjectSharing} :: ProjectDTO
              else project1Dto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO deleteProjects requestContext
      -- AND: Enabled anonymous sharing
      updateAnonymousProjectSharing requestContext anonymousSharingEnabled
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareProjectCreateDtos resBody expDto
      -- AND: Find a result in DB
      (Right eventsInDB) <- runInContextIO (findProjectEventsByProjectUuid resBody.uuid) requestContext
      if anonymousSharingEnabled
        then
          assertExistenceOfProjectInDB
            requestContext
            ( project1
                { uuid = resBody.uuid
                , description = Nothing
                , isTemplate = False
                , sharing = AnyoneWithLinkEditProjectSharing
                , projectTags = []
                , permissions = []
                , creatorUuid = Nothing
                }
                :: Project
            )
            eventsInDB
        else do
          let aPermissions =
                [ (head project1.permissions)
                    { projectUuid = resBody.uuid
                    }
                    :: ProjectPerm
                ]
          assertExistenceOfProjectInDB
            requestContext
            ( project1
                { uuid = resBody.uuid
                , description = Nothing
                , isTemplate = False
                , projectTags = []
                , permissions = aPermissions
                }
                :: Project
            )
            eventsInDB

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod reqUrl "packageId"
