module Specs.Service.KnowledgeModel.Migration.Migrator.Common where

import Data.Maybe
import qualified Data.UUID as U

import Shared.Constant.KnowledgeModel
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.Tenant.Tenant
import Shared.Util.Date

createTestMigratorStateWithEvents :: [KnowledgeModelEvent] -> [KnowledgeModelEvent] -> Maybe KnowledgeModel -> KnowledgeModelMigration
createTestMigratorStateWithEvents editorEvents targetPackageEvents mKm =
  KnowledgeModelMigration
    { editorUuid = fromJust . U.fromString $ "09080ce7-f513-4493-9583-dce567b8e9c5"
    , metamodelVersion = knowledgeModelMetamodelVersion
    , state = RunningKnowledgeModelMigrationState
    , editorPreviousPackageUuid = U.nil
    , targetPackageUuid = U.nil
    , editorPreviousPackageEvents = editorEvents
    , targetPackageEvents = targetPackageEvents
    , resultEvents = []
    , currentKnowledgeModel = mKm
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt'' 2018 1 1 1
    }
