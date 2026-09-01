module Specs.Api.Handler.Project.Detail_WS.GeneralSpec where

import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)

import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.Tenant.Tenant
import Shared.Service.Project.ProjectMapper
import Shared.Service.Project.ProjectService
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Detail_WS.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common

generalSpec requestContext =
  describe "general" $ do
    test200 requestContext
    test403 requestContext
    test404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 requestContext =
  it "WS 200 OK" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      -- WHEN
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project.uuid
      -- THEN:
      assertCountOfWebsocketConnection requestContext 3
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test403 requestContext = do
  create_403_no_perm
    "WS 403 FORBIDDEN - no required view entity permission (Anonymous, Private)"
    requestContext
    project1
    Nothing
    "View Project"
  create_403_no_perm
    "WS 403 FORBIDDEN - no required view entity permission (Non-owner, Private)"
    requestContext
    project1
    (Just reqNonAdminAuthToken)
    "View Project"
  it "WS 403 FORBIDDEN - when perms are changed" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      let updatedProject = project {visibility = PrivateProjectVisibility, sharing = RestrictedProjectSharing}
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project.uuid
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Project"
      -- WHEN: Update permission
      runInContext (modifyProjectShare updatedProject.uuid (toChangeDTO updatedProject)) requestContext
      -- THEN: Read response
      read_SetUserList c1 1
      read_SetUserList c1 0
      read_SetUserList_or_Error c2 expError
      read_SetUserList_or_Error c3 expError
      -- AND: Close sockets
      closeSockets [s1, s2, s3]

create_403_no_perm title requestContext project authToken errorMessage =
  it title $
    -- GIVEN: Prepare database
    do
      insertProjectAndUsers requestContext project
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN errorMessage
      -- WHEN: Connect to websocket
      (c1, s1) <- createConnection requestContext (reqUrlT project.uuid authToken)
      -- THEN: Read response
      read_Error c1 expError
      -- AND: Close sockets
      closeSockets [s1]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test404 requestContext = do
  it "WS 404 NOT FOUND - non existing entity" $
    -- GIVEN: Prepare request
    do
      let nonExistingProjectUuid = "fd5ea37c-852a-4174-9d65-2bf23202541d"
      -- AND: Prepare expectation
      let expError =
            NotExistsError
              ( _ERROR_DATABASE__ENTITY_NOT_FOUND
                  "project"
                  [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", nonExistingProjectUuid)]
              )
      -- WHEN:
      (c1, s1) <- createConnection requestContext (reqUrlT (u' nonExistingProjectUuid) (Just reqAuthToken))
      -- THEN:
      read_Error c1 expError
      -- AND: Close sockets
      closeSockets [s1]
  it "WS 404 NOT FOUND - project was deleted" $
    -- GIVEN: Prepare database
    do
      let project = project10
      insertProjectAndUsers requestContext project
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers requestContext project.uuid
      -- AND: Prepare expectation
      let expError = NotExistsError (_ERROR_SERVICE_PROJECT_COLLABORATION__FORCE_DISCONNECT (U.toString project.uuid))
      -- WHEN: Update permission
      runInContext (deleteProject project.uuid True) requestContext
      -- THEN: Read response
      read_Error c1 expError
      read_Error c2 expError
      read_Error c3 expError
      -- AND: Close sockets
      closeSockets [s1, s2, s3]
