module Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.GeneralSpec where

import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)

import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Service.KnowledgeModel.Editor.EditorService
import Shared.Util.Uuid

import Shared.Model.Config.WizardServerConfig
import Shared.Model.Tenant.Tenant
import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Detail_WS.Common
import Specs.Api.Handler.Websocket.Common
import Specs.Common
import WizardServer.Model.Context.RequestContext

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
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- WHEN
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      -- THEN:
      assertCountOfWebsocketConnection requestContext 2
      -- AND: Close sockets
      closeSockets [s1, s2]

---- ----------------------------------------------------
---- ----------------------------------------------------
---- ----------------------------------------------------
test403 requestContext =
  it "WS 403 FORBIDDEN" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Prepare expectation
      let expError = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Missing permission: KnowledgeModelEditorsUseRolePermission"
      -- WHEN: Connect to websocket
      (c1, s1) <- createConnection requestContext (reqUrlT editor.uuid (Just reqIsaacAuthToken))
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
      let nonExistingEditorUuid = "fd5ea37c-852a-4174-9d65-2bf23202541d"
      -- AND: Prepare expectation
      let expError =
            NotExistsError
              ( _ERROR_DATABASE__ENTITY_NOT_FOUND
                  "knowledge_model_editor"
                  [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", nonExistingEditorUuid)]
              )
      -- WHEN:
      (c1, s1) <- createConnection requestContext (reqUrlT (u' nonExistingEditorUuid) (Just reqAuthToken))
      -- THEN:
      read_Error c1 expError
      -- AND: Close sockets
      closeSockets [s1]
  it "WS 404 NOT FOUND - project was deleted" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers requestContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers requestContext editor.uuid
      -- AND: Prepare expectation
      let expError = NotExistsError (_ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__COLLABORATION__FORCE_DISCONNECT (U.toString editor.uuid))
      -- WHEN: Update permission
      runInContext (deleteEditor editor.uuid) requestContext
      -- THEN: Read response
      read_Error c1 expError
      read_Error c2 expError
      -- AND: Close sockets
      closeSockets [s1, s2]
