module Specs.Service.KnowledgeModel.Editor.Event.EditorEventServiceSpec where

import Test.Hspec

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.Tenant.Tenant
import Shared.Service.KnowledgeModel.Editor.EditorMapper
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventService
import qualified Shared.Service.KnowledgeModel.Squash.Squasher as Squasher

import Specs.Common

editorEventServiceSpec requestContext =
  describe "EditorEventService" $
    it "squashEvents squashes an editor of another tenant" $
      -- GIVEN: prepare data
      do
        let differentContext = setTenantUuid differentTenant.uuid requestContext
        let events = [e_km1_ch1_q2, e_km1_ch1_q2_second_edit]
        let editorEvents = fmap (toKnowledgeModelEditorEvent differentKnowledgeModelEditor.uuid differentTenant.uuid) events
        runInContextIO KnowledgeModelPackage.runMigration requestContext
        runInContextIO KnowledgeModelEditor.runMigration requestContext
        runInContextIO (traverse insertKnowledgeModelEvent editorEvents) differentContext
        runInContextIO (updateKnowledgeModelEditorByUuid (differentKnowledgeModelEditor {squashed = False})) differentContext
        -- AND: prepare expectation
        let expEvents = fmap (toKnowledgeModelEditorEvent differentKnowledgeModelEditor.uuid differentTenant.uuid) (Squasher.squash events)
        -- WHEN:
        runInContextIO squashEvents requestContext
        -- THEN:
        (Right resultEvents) <- runInContext (findKnowledgeModelEventsByEditorUuid differentKnowledgeModelEditor.uuid) differentContext
        resultEvents `shouldBe` expEvents
        (Right editor) <- runInContext (findKnowledgeModelEditorByUuid differentKnowledgeModelEditor.uuid) differentContext
        editor.squashed `shouldBe` True
